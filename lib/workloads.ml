open Node_intf
open Tensor_intf

let load_transformer t (module N : Node) nodes =
  let open Op_intf in
  let stats_array = Stats.stats_nodes nodes (module N) in
  let mpar = N.dev_count in
  let h = Transformer.embed_dim t in
  let num_layers = Transformer.num_layers t in
  assert (Transformer.num_heads t mod mpar = 0);
  let handle_node node_data =
    let { id = node_id; _ } = node_data in
    let handle_dev device_data =
      let Device_intf.{ id = device_id; _ } = device_data in
      let make_t s =
        Tensor.make ~node:node_data ~device:device_data ~dtype:(module BF16) s
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
      let tformer_ops =
        Base.Array.init (l_ops_count * num_layers) ~f:(fun idx ->
          layer_ops.(idx mod l_ops_count))
      in
      let w_ops = Base.Array.filter_map tformer_ops ~f:Op.is_weight_op in
      let stats = Base.Array.map w_ops ~f:Op.load_weight in
      let init = stats_array.(node_id).(device_id) in
      stats_array.(node_id).(device_id) <- Base.Array.fold stats ~init ~f:Stats.( + )
    in
    Array.iter handle_dev N.devices
  in
  Array.iter handle_node nodes;
  stats_array
;;
