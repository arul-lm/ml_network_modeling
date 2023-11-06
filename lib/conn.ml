include Conn_intf

(* conn id - vertex id *)
(* conn id is not port id *)
(* 2 nodes of 8 devices *)
(* vertex ids - 8 device ids; 8 rail ids; 1 spine id; *)
(* connections are between vertices *)
type ('a, 'b) t =
  { pair : int * int
  ; endpoints : 'a * 'b
  ; link_id : int
  }

let to_string { pair; link_id; _ } =
  let l, r = pair in
  Printf.sprintf "LinkId:%d|Conn:(%d,%d)" link_id l r
;;

let make (l, r) endpoints link_id = { pair = l, r; link_id; endpoints }

let all_to_all xs ys =
  let link_id = ref 0 in
  let make_conn src_id src =
    let form_conn dst_id tgt =
      (* if dst_id = src_id *)
      (* then None *)
      (* else ( *)
        let conn = make (src_id, dst_id) (src, tgt) !link_id in
        link_id := !link_id + 1;
        Some conn
(* ) *)
    in
    Base.Array.filter_mapi ys ~f:form_conn
  in
  let result = Array.mapi make_conn xs in
  !link_id, result
;;

let connections xs ys ~conn_type =
  match conn_type with
  | `AllToAll -> all_to_all xs ys
;;

let conn_pair t = t.pair
let link_id t = t.link_id
let endpoints t = t.endpoints
