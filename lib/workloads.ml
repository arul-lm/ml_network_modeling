open Node_intf
open Tensor_intf

let dummy (module N : Node) nodes =
  let stats_array = Stats.stats_nodes nodes (module N) in
  let handle_node node_data =
    let { id = node_id; _ } = node_data in
    let handle_dev device_data =
      let Device_intf.{ id = device_id; _ } = device_data in
      let shape = if device_id mod 4 = 0 then [ 256; 5120; 10240 ] else [256; 512*5; 1024*25] in
      let w =
        Tensor.make
          shape
          ~node:node_data
          ~device:device_data
          ~dtype:(module FP32)
        |> Option.get
      in
      let stats = Op_intf.CreateOp w |> Op.to_stats in
      let stats_node = stats_array.(node_id).(device_id) in
      stats_array.(node_id).(device_id) <- Stats.(stats_node + stats);
    in
    Array.iter handle_dev N.devices
  in
  Array.iter handle_node nodes;
  stats_array
;;
