open Node_intf

type weight_op =
  | Create of Tensor.t
  | Linear of Tensor.t * Tensor.t
  | LayerNorm of Tensor.t * Tensor.t

type no_param_op =
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
  val ( @ ) : weight_op -> t
  val ( & ) : no_param_op -> t
end
