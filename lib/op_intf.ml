open Node_intf
open Optimizer_state

type weight_op =
  | Create of Tensor.t
  | Linear of Tensor.t * Tensor.t
  | LayerNorm of Tensor.t * Tensor.t
  | QKV of weight_op

type no_param_op =
  | AV of Tensor.t
  | QK of (module Node) node_data * (module Device) device_data
  | Softmax of (module Node) node_data * (module Device) device_data

(* Need type hierarchy. Ops with weights and Ops only used in forward/backward passes *)
type op_type =
  | WeightOp of weight_op
  | NoParamOp of no_param_op

module type Op = sig
  type t

  val is_weight_op : t -> weight_op option
  val load_weight : weight_op -> Stats.t
  val load_optimizer_states : (module Optimizer) -> weight_op -> Stats.t
  val forward : (module Device) -> t -> Tensor.t -> Tensor.t * Stats.t
  val ( @ ) : weight_op -> t
  val ( & ) : no_param_op -> t
  val to_string : t -> string
end
