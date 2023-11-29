open Node_intf
open Tensor_intf

let load_transformer t (wl : Transformer_wl.t) (module N : Node) nodes ~comm_f =
  let open Base in
  let stats_array = Stats.stats_nodes nodes (module N) in
  let node_count = Array.length nodes in
  let total_devices = node_count * N.dev_count in
  let mpar = Transformer_wl.mpar_factor wl * N.dev_count in
  let dpar = total_devices / mpar in
  let optimizer = Transformer.optimizer t in
  let b = Transformer_wl.batch_size wl in
  assert (b % dpar = 0);
  let s = Transformer_wl.seq_len wl in
  let e = Transformer.embed_dim t in
  let forward_pass = Op.forward N.device in
  let comm_info_list : Op_intf.comm_info list ref = ref [] in
  let handle_node node =
    let { id = node_id; _ } = node in
    let handle_dev device =
      let Device_intf.{ id = device_id; _ } = device in
      let tf_ops =
        Transformer.build
          t
          mpar
          (b / dpar, s)
          (node, Array.length nodes)
          (device, N.dev_count)
      in
      let comp_ops = Array.filter_map tf_ops ~f:Op.is_compute_op in
      let w_ops = Array.filter_map comp_ops ~f:Op.is_weight_op in
      let comm_ops = Array.filter_map tf_ops ~f:Op.is_comm_op in
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
        let a, s2 = forward_pass op a in
        a, Stats.(s1 + s2)
      in
      let _, fwd_stats = Array.fold ~init:(act, empty_stats) ~f:run_fwd comp_ops in
      (* Comm ops - build comm_info *)
      let device_uid = (node_id * N.dev_count) + device_id in
      let run_comm acc op = Stats.add_comm acc (comm_f op) in
      let add_comm_info (Op_intf.AllReduce (_, dev_involved, _, _) as op) =
        let cur_dev_group = device_uid / dev_involved in
        let offset = cur_dev_group * dev_involved in
        let device_ids = List.init dev_involved ~f:(fun id -> offset + id) in
        comm_info_list
        := Op_intf.{ comm_op = op; src = device_uid; addrs = device_ids }
           :: !comm_info_list
      in
      let comm_stats = Array.fold ~init:empty_stats ~f:run_comm comm_ops in
      Array.iter comm_ops ~f:add_comm_info;
      (* Stdlib.Printf.printf "Lat:%f\n" (Stats.op_time fwd_stats); *)
      (* Stdlib.Printf.printf "Comm:%f\n" (Stats.comm_time comm_stats); *)
      stats_array.(node_id).(device_id)
      <- Stats.(weight_stats + opt_stats + fwd_stats + comm_stats)
    in
    Array.iter ~f:handle_dev N.devices
  in
  Array.iter ~f:handle_node nodes;
  List.rev !comm_info_list, stats_array
;;
