open Node_intf

type t

val add_node_stats
  :  node:(module Node) node_data
  -> device:(module Device) device_data
  -> mem_used:float
  -> flops:int
  -> t

val mem_used : t -> float
val stats_nodes : (module Node) node_data array -> (module Node) -> t array array
val ( + ) : t -> t -> t
val ( * ) : t -> float -> t
val node : t -> (module Node) node_data
val device : t -> (module Device) device_data
val empty : (module Node) node_data -> (module Device) device_data -> t
val add_flops : t -> int -> t
val flops : t -> int
