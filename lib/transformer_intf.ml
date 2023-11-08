open Node_intf
open Device_intf
open Tensor_intf
    
module type Transformer = sig
  type t

  val embed_dim : t -> int
  val num_heads : t -> int
  val num_layers : t -> int
  val head_dim : t -> int
  val make : embed_dim:int -> num_heads:int -> num_layers:int -> w_dtype:(module Dtype) -> t option
  val build : t -> int -> (module Node) node_data -> (module Device) device_data -> Op.t array
end
