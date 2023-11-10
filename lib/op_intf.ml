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

type compute_op =
  | WeightOp of weight_op
  | NoParamOp of no_param_op

type comm_op = AllReduce of int * int * Tensor.t

(* Need type hierarchy. Ops with weights and Ops only used in forward/backward passes *)
type op_type =
  | ComputeOp of compute_op
  | CommOp of comm_op

module type Op = sig
  type t

  val is_weight_op : compute_op -> weight_op option
  val is_compute_op : t -> compute_op option
  val is_comm_op : t -> comm_op option
  val load_weight : weight_op -> Stats.t
  val load_optimizer_states : (module Optimizer) -> weight_op -> Stats.t
  val forward : (module Device) -> compute_op -> Tensor.t -> Tensor.t * Stats.t
  val ( @ ) : weight_op -> t
  val ( & ) : no_param_op -> t
  val ( % ) : comm_op -> t
  val to_string : t -> string
end
