open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type link_type =
  | Intra
  | Inter
[@@deriving yojson]

type vertex_type =
  | Switch
  | Device
[@@deriving yojson]

type link_data =
  { source : int
  ; target : int
  ; distance : int
  ; index : int
  ; link_type : link_type
  ; link_id : int
  }
[@@deriving yojson]

type vertex_data =
  { name : string
  ; group : string
  ; index : int
  ; vertex_type : vertex_type
  }
[@@deriving yojson]

type graph =
  { vertices : vertex_data list
  ; links : link_data list
  }
[@@deriving yojson]

let link_data_of_node (module N : Node_intf.Node) =
  let (module IA) = N.intra_link in
  (* let (module IR) = N.inter_link in *)
  (* let max_bw = Int.max IA.bandwidth IR.bandwidth in *)
  let result = ref [] in
  let handle_conns link_type bw conns =
    let unwind_conn dsts =
      let make_link dst =
        let source, target = Conn.conn_pair dst in
        let link_id = Conn.link_id dst in
        let lid = List.length !result in
        result
        := { distance = bw; source; target; index = lid; link_type; link_id } :: !result
      in
      Array.iter make_link dsts
    in
    Array.iter unwind_conn conns
  in
  handle_conns Intra IA.bandwidth N.intra_connections;
  (* handle_conns Inter IR.bandwidth N.inter_connections; *)
  !result
;;

let vertex_data_of_node (module N : Node_intf.Node) node_data =
  let Node_intf.{ id = node_id; _ } = node_data in
  let result = ref [] in
  let start = N.dev_count * node_id in
  let make_vertices vertex_type name vs =
    let make_vertex id =
      let group = Printf.sprintf "%s_%d" name node_id in
      let name = Printf.sprintf "%s_%d" name id in
      let vid = start + List.length !result in
      result := { name; group; index = vid; vertex_type } :: !result
    in
    Array.iter make_vertex vs
  in
  let devices = Array.map (fun Device_intf.{ id } -> id) N.devices in
  let (module D) = N.device in
  make_vertices Device D.name devices;
  !result
;;

let serialize_level1 nodes ~file_name =
  let open Level1_intf in
  (* let _inter_conns = DGX_L1.inter_connections nodes in *)
  let nodes = Array.to_list nodes in
  let vertices = Base.List.map nodes ~f:(vertex_data_of_node DGX_L1.node) in
  let vertices = List.concat vertices in
  let g = yojson_of_graph { vertices; links = [] } in
  Yojson.Safe.to_file file_name g
;;
