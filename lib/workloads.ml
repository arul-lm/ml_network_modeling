open Node_intf

let load_transformer t (module N : Node) nodes =
  let stats_array = Stats.stats_nodes nodes (module N) in
  let mpar = N.dev_count in
  let handle_node node_data =
    let { id = node_id; _ } = node_data in
    let handle_dev device_data =
      let Device_intf.{ id = device_id; _ } = device_data in
      let tf_ops = Transformer.build t mpar node_data device_data in
      let w_ops = Base.Array.filter_map tf_ops ~f:Op.is_weight_op in
      let stats = Base.Array.map w_ops ~f:Op.load_weight in
      let init = stats_array.(node_id).(device_id) in
      stats_array.(node_id).(device_id) <- Base.Array.fold stats ~init ~f:Stats.( + )
    in
    Array.iter handle_dev N.devices
  in
  Array.iter handle_node nodes;
  stats_array
;;
