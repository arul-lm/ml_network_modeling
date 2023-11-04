include Conn_intf

type t =
  { device_pair : int * int
  ; link_id : int
  }

let to_string { device_pair; link_id } =
  let l, r = device_pair in
  Printf.sprintf "LinkId:%d|Conn:(%d,%d)" link_id l r
;;

let make (l, r) link_id = { device_pair = l, r; link_id }

let all_to_all (devices : Device_intf.device_data array) =
  let open Device_intf in
  let link_id = ref 0 in
  let make_conn { id = src_id } =
    let form_conn { id = dst_id } =
      if src_id = dst_id
      then None
      else (
        let conn = make (src_id, dst_id) !link_id in
        link_id := !link_id + 1;
        Some conn)
    in
    Base.Array.filter_map devices ~f:form_conn
  in
  let result = Array.map make_conn devices in
  result
;;

let connections devices ~conn_type =
  match conn_type with
  | `AllToAll -> all_to_all devices
;;
