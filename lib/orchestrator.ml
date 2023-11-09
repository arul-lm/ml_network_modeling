open Node_intf
open Tensor_intf

let load_transformer t (module N : Node) nodes =
  let open Base in
  let stats_array = Stats.stats_nodes nodes (module N) in
  let mpar = N.dev_count in
  let optimizer = Transformer.optimizer t in
  let wl = Transformer_wl.make ~batch_size:1 ~seq_len:512 in
  let b = Transformer_wl.batch_size wl in
  let s = Transformer_wl.seq_len wl in
  let e = Transformer.embed_dim t in
  let handle_node node =
    let { id = node_id; _ } = node in
    let handle_dev device =
      let Device_intf.{ id = device_id; _ } = device in
      let tf_ops = Transformer.build t mpar (b, s) node device in
      let w_ops = Array.filter_map tf_ops ~f:Op.is_weight_op in
      (* Load weight *)
      let stats = Array.map w_ops ~f:Op.load_weight in
      let init = stats_array.(node_id).(device_id) in
      let weight_stats = Array.fold stats ~init ~f:Stats.( + ) in
      (* Load optimizer *)
      let opt_stats =
        if Transformer.is_train t
        then Base.Array.map w_ops ~f:(Op.load_optimizer_states optimizer)
        else [| Stats.empty node device |]
      in
      let opt_stats = Array.fold opt_stats ~init ~f:Stats.( + ) in
      (* Forward pass *)
      let act =
        Tensor.make ~node ~device ~dtype:(module BF16) [ b; s; e ]
        |> Option.value_exn ~here:[%here]
      in
      let empty_stats = Stats.empty node device in
      let run_fwd (a, s1) op =
        let a, s2 = Op.forward op a in
        a, Stats.(s1 + s2)
      in
      let _, _fwd_stats = Array.fold ~init:(act, empty_stats) ~f:run_fwd tf_ops in
      stats_array.(node_id).(device_id) <- Stats.(weight_stats + opt_stats)
    in
    Array.iter ~f:handle_dev N.devices
  in
  Array.iter ~f:handle_node nodes;
  stats_array
;;
