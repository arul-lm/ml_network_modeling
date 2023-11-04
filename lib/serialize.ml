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
  ; value : int
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
  let result = ref [] in
  let handle_conns link_type bw conns =
    let unwind_conn dsts =
      let make_link dst =
        let source, target = Conn.conn_pair dst in
        let link_id = Conn.link_id dst in
        let lid = List.length !result in
        result
        := { value = bw; source; target; index = lid; link_type; link_id } :: !result
      in
      Array.iter make_link dsts
    in
    Array.iter unwind_conn conns
  in
  let (module IA) = N.intra_link in
  handle_conns Intra IA.bandwidth N.intra_connections;
  let (module IR) = N.inter_link in
  handle_conns Inter IR.bandwidth N.inter_connections;
  !result
;;

let vertex_data_of_node (module N : Node_intf.Node) =
  let result = ref [] in
  let make_vertices vertex_type name vs =
    let make_vertex id =
      let name = Printf.sprintf "%s_%d" name id in
      let vid = List.length !result in
      result := { name; group = name; index = vid; vertex_type } :: !result
    in
    Array.iter make_vertex vs
  in
  let devices = Array.map (fun Device_intf.{ id } -> id) N.devices in
  let (module D) = N.device in
  make_vertices Device D.name devices;
  let switches = Array.map (fun Switch_intf.{ id } -> id) N.switches in
  let (module S) = N.switch in
  make_vertices Switch S.name switches;
  !result
;;

let serialize_node node ~file_name =
  let links = link_data_of_node node in
  let vertices = vertex_data_of_node node in
  let g = yojson_of_graph { vertices; links } in
  Yojson.Safe.to_file file_name g
;;
