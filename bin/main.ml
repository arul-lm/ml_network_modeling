open Ml_network_modeling

let () =
  let open Level1_intf in
  let nodes = Node_intf.make_dgx 2 in
  let _inter_conns = DGX_L1.inter_connections nodes in
  ()
(* Serialize.serialize_node (module N) ~file_name:"graph.json" *)
