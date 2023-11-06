open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Level1_intf
open Node_intf

type link_type =
  | Intra
  | Inter
[@@deriving yojson]

type vertex_type =
  | RailSwitch
  | Device
[@@deriving show]

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
  ; vertex_type : string
  ; centroid : float * float
  }
[@@deriving yojson]

type graph =
  { vertices : vertex_data list
  ; links : link_data list
  }
[@@deriving yojson]

let link_data_of_node (module N : Node) node_data =
  let Node_intf.{ id = node_id; _ } = node_data in
  let (module IA) = N.intra_link in
  let link_count, intra_conns = N.intra_connections in
  let link_off = link_count * node_id in
  let dev_off = N.dev_count * node_id in
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

let device_cx = 100.0
let device_cy = 100.0
let device_col_off = 20.0
let device_row_off = 40.0
let num_levels = 2.0

let make_vertices cx cy rows row_off col_off start group vertex_type name vs =
  let result = ref [] in
  let row_count = Array.length vs / rows in
  let make_vertex dev_id =
    let name = Printf.sprintf "%s_%d" name (start + dev_id) in
    let vid = start + List.length !result in
    let row_id = dev_id / row_count in
    let col_id = dev_id mod row_count in
    let cx_off = Int.to_float row_id *. row_off in
    let cy_off = Int.to_float col_id *. col_off in
    let centroid = cx +. cx_off, cy +. cy_off in
    result := { name; group; index = vid; vertex_type; centroid } :: !result
  in
  Array.iter make_vertex vs;
  !result
;;

let vertex_data_of_l1 nodes (module L1 : Level1) =
  let (module S) = L1.switch in
  let (module N) = L1.node in
  let node_count = List.length nodes in
  let start = node_count * N.dev_count in
  let cx = device_cx /. num_levels in
  let min_cy = device_cy in
  let max_cy =
    (Int.to_float node_count *. device_cy) +. (device_col_off *. Int.to_float N.dev_count)
  in
  let col_off = (max_cy -. min_cy) /. Int.to_float L1.switch_count in
  let cy = min_cy in
  let rows = 1 in
  let row_off = 0. in
  let switches = Array.map (fun Switch_intf.{ id } -> id) L1.switches in
  let vertex_type = show_vertex_type RailSwitch in
  let group = S.name in
  make_vertices cx cy rows row_off col_off start group vertex_type S.name switches
;;

let vertex_data_of_node (module N : Node) node_data =
  let Node_intf.{ id = node_id; _ } = node_data in
  let start = N.dev_count * node_id in
  let cx = device_cx in
  let cy = device_cy *. Int.to_float node_id in
  let rows = 2 in
  let row_off = device_row_off in
  let col_off = device_col_off in
  let devices = Array.map (fun Device_intf.{ id } -> id) N.devices in
  let (module D) = N.device in
  let vertex_type = show_vertex_type Device in
  let group = Printf.sprintf "%s_%d" vertex_type node_id in
  make_vertices cx cy rows row_off col_off start group vertex_type D.name devices
;;

let serialize_level1 nodes ~file_name =
  let nodes = Array.to_list nodes in
  let vertices =
    Base.List.map nodes ~f:(vertex_data_of_node DGX_L1.node) |> List.concat
  in
  let links = Base.List.map nodes ~f:(link_data_of_node DGX_L1.node) |> List.concat in
  let vertices = vertex_data_of_l1 nodes (module DGX_L1) @ vertices in
  let g = yojson_of_graph { vertices; links } in
  Yojson.Safe.to_file file_name g
;;
