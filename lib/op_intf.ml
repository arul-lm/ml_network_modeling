open Node_intf

(* Need type hierarchy. Ops with weights and Ops only used in forward/backward passes *)
type op_type =
  | CreateOp of Tensor.t
  | Linear of Tensor.t * Tensor.t
  | LayerNorm of Tensor.t * Tensor.t
  | Transpose of (module Node) node_data * (module Device) device_data
  | Matmul of (module Node) node_data * (module Device) device_data

module type Op = sig
  type t = op_type

  val to_stats : t -> Stats.t
end
