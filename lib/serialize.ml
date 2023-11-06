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
        let link_id = link_off + Conn.link_id dst in
        result := { distance = bw; source; target; index = link_id; link_type } :: !result
      in
      Array.iter make_link dsts
    in
    Array.iter unwind_conn conns
  in
  handle_conns Intra IA.bandwidth intra_conns;
  List.rev !result
;;

let link_data_of_l1 nodes (module L1 : Level1) =
  let _, inter_conns = L1.inter_connections nodes in
  let (module IR) = L1.inter_link in
  let (module N) = L1.node in
  let intra_count, _ = N.intra_connections in
  let result = ref [] in
  let link_off = intra_count * Array.length nodes in
  let dev_off = N.dev_count * Array.length nodes in
  let handle_conns link_type bw conns =
    let unwind_conn dsts =
      let make_link dst =
        let source, target = Conn.conn_pair dst in
        let Node_intf.{ id = node_id } = nodes.(target) in
        let target = (node_id * N.dev_count) + source in
        let source = dev_off + source in
        (* Printf.printf "%d,%d\n" source target; *)
        let link_id = link_off + Conn.link_id dst in
        result := { distance = bw; source; target; index = link_id; link_type } :: !result
      in
      Array.iter make_link dsts
    in
    Array.iter unwind_conn conns
  in
  handle_conns Inter IR.bandwidth inter_conns;
  List.rev !result
;;

let node_size = 12.0
let device_row_off = node_size *. 4.
let device_col_off = node_size *. 2.
let device_rows = 2
let device_row_span = (node_size +. device_row_off) *. Int.to_float device_rows

let device_col_span device_count =
  (node_size +. device_col_off) *. Int.to_float (device_count / device_rows)
;;

let num_levels = 2.0
let device_cx = device_row_span *. num_levels

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
  List.rev !result
;;

let vertex_data_of_l1 nodes (module L1 : Level1) =
  let (module S) = L1.switch in
  let (module N) = L1.node in
  let node_count = List.length nodes in
  let start = node_count * N.dev_count in
  let cx = device_cx /. num_levels in
  let min_cy = 0. in
  let max_cy = Int.to_float node_count *. device_col_span (N.dev_count + 2) in
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
  let cy = device_col_span (N.dev_count + 2) *. Int.to_float node_id in
  let rows = device_rows in
  let row_off = device_row_off in
  let col_off = device_col_off in
  let devices = Array.map (fun Device_intf.{ id } -> id) N.devices in
  let (module D) = N.device in
  let vertex_type = show_vertex_type Device in
  let group = Printf.sprintf "%s_%d" vertex_type node_id in
  make_vertices cx cy rows row_off col_off start group vertex_type D.name devices
;;

let serialize_level1 nodes ~file_name =
  let nodes_l = Array.to_list nodes in
  let vertices =
    Base.List.map nodes_l ~f:(vertex_data_of_node DGX_L1.node) |> List.concat
  in
  let links = Base.List.map nodes_l ~f:(link_data_of_node DGX_L1.node) |> List.concat in
  let vertices = vertices @ vertex_data_of_l1 nodes_l (module DGX_L1) in
  let links = links @ link_data_of_l1 nodes (module DGX_L1) in
  let g = yojson_of_graph { vertices; links } in
  Yojson.Safe.to_file file_name g
;;
