open Node_intf
open Device_intf

type t =
  { node : (module Node) node_data
  ; device : (module Device) device_data
  ; mem_used : float
  ; flops : int
  ; op_time : float
  ; comm_time : float
  }

let add_node_stats ~node ~device ~mem_used ~flops ~op_time ~comm_time =
  { node; device; mem_used; flops; op_time; comm_time }
;;

let mem_used t = t.mem_used

let empty node device =
  add_node_stats ~node ~device ~mem_used:0. ~flops:0 ~op_time:0. ~comm_time:0.
;;

let stats_nodes nodes (module N : Node) =
  let handle_node node_data =
    let handle_dev device_data = empty node_data device_data in
    Array.map handle_dev N.devices
  in
  Array.map handle_node nodes
;;

exception InvalidStatsAddition

let ( + ) t1 t2 =
  if t1.node <> t2.node || t1.device <> t2.device
  then raise InvalidStatsAddition
  else
    { t1 with
      mem_used = t1.mem_used +. t2.mem_used
    ; flops = t1.flops + t2.flops
    ; op_time = t1.op_time +. t2.op_time
    ; comm_time = t1.comm_time +. t2.comm_time
    }
;;

let ( * ) t n =
  { t with
    mem_used = t.mem_used *. n
  ; flops = t.flops * Int.of_float n
  ; op_time = t.op_time *. n
  ; comm_time = t.comm_time *. n
  }
;;

let device t = t.device
let node t = t.node

let add_flops t flops =
  let ff = t.flops in
  let flops = Int.add flops ff in
  { t with flops }
;;

let add_mem t mem_used = { t with mem_used = t.mem_used +. mem_used }
let add_op_time t op_time = { t with op_time = t.op_time +. op_time }
let add_comm t comm_time = { t with comm_time = t.comm_time +. comm_time }
let flops t = t.flops
let op_time t = t.op_time
let comm_time t = t.comm_time
