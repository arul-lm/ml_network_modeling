open  Ppx_yojson_conv_lib.Yojson_conv.Primitives
        
type link_data =
  { source : int
  ; target : int
  ; value : int
  ; index : int
  }
[@@deriving yojson]

type list_of_link_data = link_data list [@@deriving yojson]
    
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
  
let serialize_links node ~file_name =
  let link_data = link_data_of_node node in
  let ser_links = yojson_of_list_of_link_data link_data in
  Yojson.Safe.to_file file_name ser_links
  
