type t =
  { device_pair : int * int
  ; link_id : int
  }

let to_string { device_pair; link_id } =
  let l, r = device_pair in
  Printf.sprintf "LinkId:%d|Conn:(%d,%d)" link_id l r
;;

let make (l, r) link_id = { device_pair = l, r; link_id }
