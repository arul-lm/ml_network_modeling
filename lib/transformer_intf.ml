open Node_intf
open Device_intf
open Tensor_intf
open Optimizer_state

module type Transformer = sig
  type t

  val embed_dim : t -> int
  val num_heads : t -> int
  val num_layers : t -> int
  val head_dim : t -> int
  val is_train : t -> bool
  val optimizer : t -> (module Optimizer)

  val make
    :  embed_dim:int
    -> num_heads:int
    -> num_layers:int
    -> w_dtype:(module Dtype)
    -> is_train:bool
    -> optimizer:(module Optimizer)
    -> t option

  val build
    :  t
    -> int
    -> int * int
    -> (module Node) node_data * int
    -> (module Device) device_data * int
    -> Op.t array
end
