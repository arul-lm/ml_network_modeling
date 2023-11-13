open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Level1_intf
open Level2_intf
open Node_intf
open! Tensor_intf

type link_type =
  | Intra
  | Inter
[@@deriving show]

type vertex_type =
  | SpineSwitch
  | RailSwitch
  | Device
[@@deriving show]

type link_data =
  { source : int
  ; target : int
  ; index : int
  ; link_type : string
  ; stroke_width : float
  ; bandwidth : float
  }
[@@deriving yojson]

type vertex_data =
  { name : string
  ; group : string
  ; index : int
  ; vertex_type : string
  ; centroid : float * float
  ; mem_used : float
  ; mem_cap : float
  }
[@@deriving yojson]

type graph =
  { vertices : vertex_data list
  ; links : link_data list
  }
[@@deriving yojson]

(* {mem_used; latency; flops; comm_time} *)
type usage_stats =
  { mem_used : float
  ; latency : float
  ; flops : int
  }

type comm_op_stats =
  { op_name : string
  ; size_in_bytes : float
  ; devices_involved : int
  ; comm_time : float
  }
[@@deriving yojson, make]

type comm_stats =
  { model : string
  ; node_count : int
  ; device_count : int
  ; comm_op_stats : comm_op_stats list
  }
[@@deriving yojson, make]

let sanitize_show_str str = String.split_on_char '.' str |> List.rev |> List.hd

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
        result
        := { source
           ; target
           ; index = link_id
           ; link_type
           ; stroke_width = bw /. IA.bandwidth *. 2.
           ; bandwidth = bw /. Int64.to_float Units.giga_b (* Gbps *)
           }
           :: !result
      in
      Array.iter make_link dsts
    in
    Array.iter unwind_conn conns
  in
  handle_conns IA.name IA.bandwidth intra_conns;
  List.rev !result
;;

let link_data_of_l1 nodes (module L1 : Level1) =
  let _, inter_conns = L1.inter_connections nodes in
  let (module IR) = L1.inter_link in
  let (module N) = L1.node in
  let (module IA) = N.intra_link in
  let intra_count, _ = N.intra_connections in
  let result = ref [] in
  let link_off = intra_count * Array.length nodes in
  let dev_off = N.dev_count * Array.length nodes in
  let handle_conns link_type bw conns =
    let unwind_conn dsts =
      let make_link dst =
        let source, target = Conn.conn_pair dst in
        let Node_intf.{ id = node_id; _ } = nodes.(target) in
        let target = (node_id * N.dev_count) + source in
        let source = dev_off + source in
        let link_id = link_off + Conn.link_id dst in
        result
        := { source
           ; target
           ; index = link_id
           ; link_type
           ; stroke_width = bw /. IA.bandwidth *. 2.
           ; bandwidth = bw /. Int64.to_float Units.giga_b
           }
           :: !result
      in
      Array.iter make_link dsts
    in
    Array.iter unwind_conn conns
  in
  (* let link_type = (sanitize_show_str (show_link_type Inter)) in *)
  handle_conns IR.name IR.bandwidth inter_conns;
  List.rev !result
;;

let link_data_of_l2 nodes (module L2 : Level2) =
  let _, inter_conns = L2.inter_connections in
  let (module IR) = L2.inter_link in
  let (module L1) = L2.l1 in
  let (module N) = L1.node in
  let (module IA) = N.intra_link in
  let intra_count, _ = N.intra_connections in
  let l1_count, _ = L1.inter_connections nodes in
  let result = ref [] in
  let link_off = (intra_count * Array.length nodes) + l1_count in
  let tgt_off = N.dev_count * Array.length nodes in
  let src_off = tgt_off + L1.switch_count in
  let handle_conns link_type bw conns =
    let unwind_conn dsts =
      let make_link dst =
        let source, target = Conn.conn_pair dst in
        let source, target = src_off + source, tgt_off + target in
        let link_id = link_off + Conn.link_id dst in
        result
        := { source
           ; target
           ; index = link_id
           ; link_type
           ; stroke_width = bw /. IA.bandwidth *. 2.
           ; bandwidth = bw
           }
           :: !result
      in
      Array.iter make_link dsts
    in
    Array.iter unwind_conn conns
  in
  handle_conns IR.name IR.bandwidth inter_conns;
  List.rev !result
;;

let node_size = 12.0
let device_row_off = node_size *. 6.
let device_col_off = node_size *. 3.
let device_rows = 2
let device_row_span = (node_size +. device_row_off) *. Int.to_float device_rows
let device_off_count = 1

let device_col_span device_count =
  (node_size +. device_col_off) *. Int.to_float (device_count / device_rows)
;;

let num_levels = 2.0
let device_cx = device_row_span *. num_levels

let make_vertices cx cy rows row_off col_off start group vertex_type name vs =
  let result = ref [] in
  let row_count = Array.length vs / rows in
  let make_vertex (dev_id, mem_used, mem_cap) =
    let name = Printf.sprintf "%s_%d" name dev_id in
    let vid = start + List.length !result in
    let row_id = dev_id / row_count in
    let col_id = dev_id mod row_count in
    let cx_off = Int.to_float row_id *. row_off in
    let cy_off = Int.to_float col_id *. col_off in
    let centroid = cx +. cx_off, cy +. cy_off in
    let giga_float = Int64.to_float Units.giga_b in
    let mem_used = mem_used /. giga_float in
    let mem_cap = mem_cap /. giga_float in
    result
    := { name; group; index = vid; vertex_type; centroid; mem_used; mem_cap } :: !result
  in
  Array.iter make_vertex vs;
  List.rev !result
;;

let vertex_data_of_l2 nodes (module L2 : Level2) =
  let (module S) = L2.switch in
  let (module L1) = L2.l1 in
  let (module N) = L1.node in
  let node_count = List.length nodes in
  let start = (node_count * N.dev_count) + L1.switch_count in
  let level_id = 2. in
  let cx = device_cx -. (device_cx /. num_levels *. level_id) in
  let min_cy = 0. in
  let max_cy =
    Int.to_float node_count *. device_col_span (N.dev_count + device_off_count)
  in
  let y_size = max_cy -. min_cy in
  let col_off = y_size /. Int.to_float L2.switch_count in
  let cy = if y_size = col_off then y_size /. 2. else min_cy in
  let rows = 1 in
  let row_off = 0. in
  let switches = Array.map (fun Switch_intf.{ id } -> id, 0., 0.) L2.switches in
  let vertex_type = show_vertex_type SpineSwitch |> sanitize_show_str in
  let group = S.name in
  make_vertices cx cy rows row_off col_off start group vertex_type S.name switches
;;

let vertex_data_of_l1 nodes (module L1 : Level1) =
  let (module S) = L1.switch in
  let (module N) = L1.node in
  let node_count = List.length nodes in
  let start = node_count * N.dev_count in
  let level_id = 1. in
  let cx = device_cx -. (device_cx /. num_levels *. level_id) in
  let min_cy = 0. in
  let max_cy =
    Int.to_float node_count *. device_col_span (N.dev_count + device_off_count)
  in
  let col_off = (max_cy -. min_cy) /. Int.to_float L1.switch_count in
  let cy = min_cy in
  let rows = 1 in
  let row_off = 0. in
  let switches = Array.map (fun Switch_intf.{ id } -> id, 0., 0.) L1.switches in
  let vertex_type = show_vertex_type RailSwitch |> sanitize_show_str in
  let group = S.name in
  make_vertices cx cy rows row_off col_off start group vertex_type S.name switches
;;

let vertex_data_of_node (module N : Node) (node_stats : Stats.t array) =
  let Node_intf.{ id = node_id; _ } = Stats.node node_stats.(0) in
  let (module D) = N.device in
  let devices =
    Array.map
      (fun s ->
        let Device_intf.{ id } = Stats.device s in
        id, Stats.mem_used s, D.memory)
      node_stats
  in
  let start = N.dev_count * node_id in
  let cx = device_cx in
  let cy = device_col_span (N.dev_count + device_off_count) *. Int.to_float node_id in
  let rows = device_rows in
  let row_off = device_row_off in
  let col_off = device_col_off in
  let vertex_type = show_vertex_type Device |> sanitize_show_str in
  let group = Printf.sprintf "%s_%d" vertex_type node_id in
  make_vertices cx cy rows row_off col_off start group vertex_type D.name devices
;;

let serialize_comm model node_count device_count comm_ops ~f =
  let open Base in
  let ht = Hashtbl.create ~size:32 (module String) in
  let handle_comm = function
    | Op_intf.AllReduce (op_name, dim1, _dim2, t) as op ->
      let comm_time = f op in
      let size = Tensor.size t in
      Hashtbl.add_multi ht ~key:op_name ~data:(comm_time, size, dim1)
  in
  Array.iter comm_ops ~f:handle_comm;
  let op_stats = ref [] in
  let handle_op_stats ~key ~data =
    let comm_op_stats =
      List.mapi data ~f:(fun id (comm_time, size, dev) ->
        let op_name = key ^ "_" ^ Int.to_string id in
        make_comm_op_stats ~op_name ~size_in_bytes:size ~devices_involved:dev ~comm_time)
    in
    op_stats := comm_op_stats :: !op_stats
  in
  Hashtbl.iteri ht ~f:handle_op_stats;
  let comm_op_stats = List.concat !op_stats in
  make_comm_stats ~model ~node_count ~device_count ~comm_op_stats ()
  |> yojson_of_comm_stats
  |> Yojson.Safe.to_file "comms.json"
;;

type time_stats =
  { type_ : string
  ; time : float
  }
[@@deriving yojson]

let serialize_time nodes model wl =
  let _comm_ops, stats_array =
    Orchestrator.load_transformer model wl DGX_L1.node nodes ~comm_f:Clos.handle_comm
  in
  let stat = stats_array.(0).(0) in
  let compute_time = Stats.op_time stat *. 2. in
  let comm_time = Stats.comm_time stat in
  let xs = [ { type_ = "compute"; time = compute_time }
  ; { type_ = "comm"; time = comm_time }
  ; { type_ = "total"; time = compute_time +. comm_time }
  ] in
  yojson_of_list (yojson_of_time_stats) xs |> Yojson.Safe.pretty_to_string
    
;;

let serialize_clos_dgx nodes model wl ~file_name =
  let _comm_ops, stats_array =
    Orchestrator.load_transformer model wl DGX_L1.node nodes ~comm_f:Clos.handle_comm
  in
  let (module N) = DGX_L1.node in
  (* serialize_comm *)
  (*   (Transformer.name model) *)
  (*   (Array.length nodes) *)
  (*   N.dev_count *)
  (*   comm_ops *)
  (*   ~f:Clos.handle_comm; *)
  let nodes_l = Array.to_list nodes in
  let vertices =
    Base.Array.fold stats_array ~init:[] ~f:(fun acc s ->
      vertex_data_of_node DGX_L1.node s :: acc)
    |> List.concat
  in
  let links = Base.List.map nodes_l ~f:(link_data_of_node DGX_L1.node) |> List.concat in
  let vertices = vertices @ vertex_data_of_l1 nodes_l (module DGX_L1) in
  let vertices = vertices @ vertex_data_of_l2 nodes_l (module Clos) in
  let links = links @ link_data_of_l1 nodes (module DGX_L1) in
  let links = links @ link_data_of_l2 nodes (module Clos) in
  let g = yojson_of_graph { vertices; links } in
  Yojson.Safe.to_file file_name g;
  let node_data = yojson_of_list yojson_of_vertex_data vertices in
  let link_data = yojson_of_list yojson_of_link_data links in
  let node_data = Yojson.Safe.pretty_to_string node_data in
  let link_data = Yojson.Safe.pretty_to_string link_data in
  node_data, link_data
;;
(* let write_to_ch out_ch = Yojson.Safe.to_channel out_ch g in *)
(* Stdlib.Out_channel.with_open_text file_name write_to_ch *)

(* Yojson.Safe.pretty_to_string g |> Stdlib.Printf.printf "%s\n" *)
