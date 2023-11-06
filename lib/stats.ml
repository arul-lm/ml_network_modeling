open Node_intf
open Device_intf

type t =
  { node : (module Node) node_data
  ; device : (module Device) device_data
  ; mem_used : float
  }

let add_node_stats ~node ~device ~mem_used = { node; device; mem_used }
let mem_used t = t.mem_used

let stats_nodes nodes (module N : Node) =
  let handle_node node_data =
    let handle_dev device_data =
      { node = node_data; device = device_data; mem_used = 0. }
    in
    Array.map handle_dev N.devices
  in
  Array.map handle_node nodes
;;

exception InvalidStatsAddition

let ( + ) t1 t2 =
  if t1.node <> t2.node || t1.device <> t2.device
  then raise InvalidStatsAddition
  else { t1 with mem_used = t1.mem_used +. t2.mem_used }
;;

let device t = t.device
let node t = t.node
