open Node_intf
open Tensor_intf

let dummy (module N : Node) nodes =
  let stats_array = Stats.stats_nodes nodes (module N) in
  let handle_node node_data =
    let { id = node_id; _ } = node_data in
    let handle_dev device_data =
      let Device_intf.{ id = device_id; _ } = device_data in
      let shape =
        if device_id mod 4 = 0 then [ 256; 5120; 10240 ] else [ 256; 512 * 5; 1024 * 25 ]
      in
      let w =
        Tensor.make shape ~node:node_data ~device:device_data ~dtype:(module FP32)
        |> Option.get
      in
      let stats = Op_intf.CreateOp w |> Op.to_stats in
      let stats_node = stats_array.(node_id).(device_id) in
      stats_array.(node_id).(device_id) <- Stats.(stats_node + stats)
    in
    Array.iter handle_dev N.devices
  in
  Array.iter handle_node nodes;
  stats_array
;;

let load_transformer (module N : Node) nodes =
  let open Op_intf in
  let stats_array = Stats.stats_nodes nodes (module N) in
  let h = 1024 in
  let s = 512 in
  let num_layers = 96 in
  let mpar = N.dev_count in
  (* let dpar = Array.length nodes in *)
  let handle_node node_data =
    let { id = node_id; _ } = node_data in
    let handle_dev device_data =
      let Device_intf.{ id = device_id; _ } = device_data in
      let make_t s =
        Tensor.make ~node:node_data ~device:device_data ~dtype:(module FP16) s
        |> Option.get
      in
      (* QKV projections *)
      let shape = [ h; h / mpar ] in
      let w_q = Linear (make_t shape) in
      (*  Out projections *)
      let shape = [ s / mpar; h ] in
      let w_o = Linear (make_t shape) in
      (* MLP *)
      let shape = [ h; 4 * h / mpar ] in
      let mlp_0 = Linear (make_t shape) in
      let shape = [ 4 * h / mpar; h ] in
      let mlp_1 = Linear (make_t shape) in
      let layer_ops = [| w_q; w_q; w_q; w_o; mlp_0; mlp_1 |] in
      let l_ops_count = Array.length layer_ops in
      let tformer_ops =
        Base.Array.init (l_ops_count * num_layers) ~f:(fun idx ->
          layer_ops.(idx mod l_ops_count))
      in
      let stats = Base.Array.map tformer_ops ~f:Op.to_stats in
      let init = stats_array.(node_id).(device_id) in
      stats_array.(node_id).(device_id) <- Base.Array.fold stats ~init ~f:Stats.( + )
    in
    Array.iter handle_dev N.devices
  in
  Array.iter handle_node nodes;
  stats_array
;;
