open Node_intf

type t

val add_node_stats
  :  node:(module Node) node_data
  -> device:(module Device) device_data
  -> mem_used:float
  -> t

val mem_used : t -> float
val stats_nodes : (module Node) node_data array -> (module Node) -> t array array
val ( + ) : t -> t -> t
val node : t -> (module Node) node_data
val device : t -> (module Device) device_data
