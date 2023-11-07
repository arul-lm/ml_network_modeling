open Node_intf

(* Need type hierarchy. Ops with weights and Ops only used in forward/backward passes *)
type op_type =
  | CreateOp of Tensor.t
  | Linear of Tensor.t * Tensor.t
  | LayerNorm of Tensor.t * Tensor.t
  | QK of (module Node) node_data * (module Device) device_data
  | Softmax of (module Node) node_data * (module Device) device_data

module type Op = sig
  type t = op_type

  val load_op : t -> Stats.t
end
