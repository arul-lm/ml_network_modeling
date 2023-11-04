open  Ppx_yojson_conv_lib.Yojson_conv.Primitives
        
type link_data =
  { source : int
  ; target : int
  ; value : int
  ; index : int
  }
[@@deriving yojson]

type vertex_data = {
  name: string;
  index: int
}[@@deriving yojson]

type graph = {
  vertices: vertex_data list;
  links: link_data list
} [@@deriving yojson]
    
let link_data_of_node ((module N) : (module Node_intf.Node)) =
  let conns = N.connections in
  let intra_bw = N.intra_link_bw in
  let result = ref [] in
  let unwind_conn dsts =
    let make_link dst =
      let source, target = Conn.device_pair dst in
      let index = Conn.link_id dst in
      result := {value = intra_bw; source; target; index} :: !result
    in
    Array.iter make_link dsts
  in
  Array.iter unwind_conn conns;
  !result

let vertex_data_of_node ((module N) : (module Node_intf.Node)) =
  let devices = N.devices in
  let (module D) = N.device in
  let device_name = D.name in
  let result = ref [] in
  let open Device_intf in
  let make_vertex {id} =
    let name = Printf.sprintf "%s_%d" device_name id in
    result := {name; index = id} :: !result
  in
  Array.iter make_vertex devices;
  !result
  
let serialize_links node ~file_name =
  let links = link_data_of_node node in
  let vertices = vertex_data_of_node node in
  let g = yojson_of_graph {vertices; links} in
  Yojson.Safe.to_file file_name g
  
