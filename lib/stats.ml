open Node_intf
open Device_intf

type t =
  { node : (module Node) node_data
  ; device : (module Device) device_data
  ; mem_used : float
  ; flops : int
  }

let add_node_stats ~node ~device ~mem_used ~flops = { node; device; mem_used; flops }
let mem_used t = t.mem_used
let empty node device = add_node_stats ~node ~device ~mem_used:0. ~flops:0

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
  else { t1 with mem_used = t1.mem_used +. t2.mem_used; flops = t1.flops + t2.flops }
;;

let ( * ) t n = { t with mem_used = t.mem_used *. n; flops = t.flops * 3 }
let device t = t.device
let node t = t.node
let add_flops t flops = { t with flops }
let flops t = t.flops
