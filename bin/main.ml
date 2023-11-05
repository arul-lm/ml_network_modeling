open Ml_network_modeling

let () =
  let open Switch_intf in
  let open Node_intf in
  let nodes = Node_intf.make_nodes (module DGX) 2 in
  let { node = (module N0); _ } = nodes.(0) in
  let rail_count = N0.dev_count in
  let rails = Switch_intf.make_rails (module Rail) rail_count in
  let make_rail_conn rail_id rail_data =
    let { rail = (module R); _ } = rail_data in
    let make_node_conn _ node_data =
      let { node = (module N); _ } = node_data in
      let dev_data = N.devices.(rail_id) in
      let _ = R.add_conn node_data dev_data in
      ()
    in
    Array.iteri make_node_conn nodes
  in
  Array.iteri make_rail_conn rails;
  
    (* let rail_conns = ref [] in *)
  (* (rail_data, (node_data, device_data) Conn.t) *)
    
;;
(* rail -> node *)
(* (\* let (module N) = (module Node.DGX : Node_intf.Node) in *\) *)
(* Serialize.serialize_node (module N) ~file_name:"graph.json" *)
