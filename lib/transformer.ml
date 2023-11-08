open Tensor_intf
type t =
  { embed_dim : int
  ; num_heads : int
  ; num_layers : int
  ; w_dtype : (module Dtype)
  }

let embed_dim t = t.embed_dim
let num_heads t = t.num_heads
let num_layers t = t.num_layers
let head_dim t = t.embed_dim / t.num_heads

let make ~embed_dim ~num_heads ~num_layers ~w_dtype =
  let t = { embed_dim; num_heads; num_layers; w_dtype} in
  let rem = embed_dim mod num_heads in
  if rem <> 0 then None else Some t
;;

let build t mpar node_data device_data =
    let h = embed_dim t in
  assert (num_heads t mod mpar = 0);
      let make_t s =
        Tensor.make ~node:node_data ~device:device_data ~dtype:(t.w_dtype) s
        |> Option.get
      in
      (* layer_norm *)
      let weight, bias = [ h ], [ h ] in
      let lnorm = LayerNorm (make_t weight, make_t bias) |> Op.( @ ) in
      (* QKV projections *)
      let weight, bias = [ h; h / mpar ], [ h / mpar ] in
      let w_q = Linear (make_t weight, make_t bias) |> Op.( @ ) in
      (*  Out projections *)
      let weight, bias = [ h / mpar; h ], [ h ] in
      let w_o = Linear (make_t weight, make_t bias) |> Op.( @ ) in
      (* MLP *)
      let weight, bias = [ h; 4 * h / mpar ], [ 4 * h / mpar ] in
      let mlp_0 = Linear (make_t weight, make_t bias) |> Op.( @ ) in
      let weight, bias = [ 4 * h / mpar; h ], [ h ] in
      let mlp_1 = Linear (make_t weight, make_t bias) |> Op.( @ ) in
      let layer_ops =
        [| lnorm
         ; w_q
         ; w_q
         ; w_q
         ; QK (node_data, device_data) |> Op.( & ) (* QK^T *)
         ; Softmax (node_data, device_data) |> Op.( & )
         ; w_o
         ; lnorm
         ; mlp_0
         ; mlp_1
        |]
      in
      let l_ops_count = Array.length layer_ops in
        Base.Array.init (l_ops_count * num_layers t) ~f:(fun idx ->
          layer_ops.(idx mod l_ops_count))
