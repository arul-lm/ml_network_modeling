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
  ; centroid : float * float
  }
[@@deriving yojson]

type graph =
  { vertices : vertex_data list
  ; links : link_data list
  }
[@@deriving yojson]

let link_data_of_node (module N : Node_intf.Node) node_data =
  let Node_intf.{ id = node_id; _ } = node_data in
  let (module IA) = N.intra_link in
  let link_count, intra_conns = N.intra_connections in
  let link_off = link_count * node_id in
  let dev_off = (N.dev_count * node_id) in
  let result = ref [] in
  let handle_conns link_type bw conns =
    let unwind_conn dsts =
      let make_link dst =
        let source, target = Conn.conn_pair dst in
        let source, target = source + dev_off, target + dev_off in
        let link_id = Conn.link_id dst in
        let lid = link_off + List.length !result in
        result
        := { distance = bw; source; target; index = lid; link_type; link_id } :: !result
      in
      Array.iter make_link dsts
    in
    Array.iter unwind_conn conns
  in
  handle_conns Intra IA.bandwidth intra_conns;
  !result
;;

let vertex_data_of_node (module N : Node_intf.Node) node_data =
  let Node_intf.{ id = node_id; _ } = node_data in
  let result = ref [] in
  let start = N.dev_count * node_id in
  let cx = 100.0 in
  let cy = 100.0 *. Int.to_float node_id in
  let rows = 2 in
  let row_count = N.dev_count / rows in
  let row_off = 40 in
  let col_off = 20 in
  let make_vertices vertex_type name vs =
    let make_vertex dev_id =
      let group = Printf.sprintf "high_bw_domain_%d" node_id in
      let name = Printf.sprintf "%s_%d" name (start + dev_id) in
      let vid = start + List.length !result in
      let row_id = dev_id / row_count in
      let col_id = dev_id mod row_count in
      let cx_off = row_id * row_off in
      let cy_off = col_id * col_off in
      let centroid = cx +. Int.to_float cx_off, cy +. Int.to_float cy_off in
      result := { name; group; index = vid; vertex_type; centroid } :: !result
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
  let vertices =
    Base.List.map nodes ~f:(vertex_data_of_node DGX_L1.node) |> List.concat
  in
  let links = Base.List.map nodes ~f:(link_data_of_node DGX_L1.node) |> List.concat in
  let g = yojson_of_graph { vertices; links} in
  Yojson.Safe.to_file file_name g
;;
