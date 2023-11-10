open Tensor_intf
open Optimizer_state

type t =
  { embed_dim : int
  ; num_heads : int
  ; num_layers : int
  ; w_dtype : (module Dtype)
  ; is_train : bool
  ; optimizer : (module Optimizer)
  ; vocab_size : int
  }

let embed_dim t = t.embed_dim
let num_heads t = t.num_heads
let num_layers t = t.num_layers
let head_dim t = t.embed_dim / t.num_heads
let is_train t = t.is_train
let optimizer t = t.optimizer
let vocab_size t = t.vocab_size

let make ~embed_dim ~num_heads ~num_layers ~w_dtype ~is_train ~optimizer ~vocab_size =
  let t =
    { embed_dim; num_heads; num_layers; w_dtype; is_train; optimizer; vocab_size }
  in
  let rem = embed_dim mod num_heads in
  if rem <> 0 then None else Some t
;;

let build t mpar (batch, seq) (node, node_count) (device, dev_count) =
  let h = embed_dim t in
  assert (num_heads t mod mpar = 0);
  assert (mpar mod dev_count = 0);
  let make_t s = Tensor.make ~node ~device ~dtype:t.w_dtype s |> Option.get in
  let act = make_t [ batch; seq; h ] in
  (* layer_norm *)
  let weight, bias = [ h ], [ h ] in
  let lnorm = LayerNorm (make_t weight, make_t bias) |> Op.( @ ) in
  (* QKV projections *)
  let weight, bias = [ h; h / mpar ], [ h / mpar ] in
  let w_q = Op_intf.Linear (make_t weight, make_t bias) in
  let qkv = QKV w_q |> Op.( @ ) in
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
     ; qkv
     ; QK (node, device) |> Op.( & ) (* QK^T *)
     ; Softmax (node, device) |> Op.( & )
     ; AV (make_t [ batch; seq; h / mpar ]) |> Op.( & )
     ; w_o
     ; AllReduce (mpar, dev_count, act) |> Op.( % )
     ; lnorm
     ; mlp_0
     ; mlp_1
     ; AllReduce (mpar, dev_count, act) |> Op.( % )
    |]
  in
  let l_ops_count = Array.length layer_ops in
  let total_ops =
    Base.Array.init
      (l_ops_count * num_layers t)
      ~f:(fun idx -> layer_ops.(idx mod l_ops_count))
  in
  (* TODO: Replace last dim should be vocab *)
  let total_devices = node_count * dev_count in
  let dpar = total_devices / mpar in
  let final_loss_shard = make_t [ batch; seq; h / dpar ] in
  let final_loss = make_t [ batch; seq; h ] in
  if is_train t
  then
    Array.append
      total_ops
      [| AllReduce (mpar, dev_count, final_loss_shard) |> Op.( % )
       ; AllReduce (total_devices, mpar, final_loss) |> Op.( % )
      |]
  else layer_ops
;;
