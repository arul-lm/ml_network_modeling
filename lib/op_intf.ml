type op_type =
  | CreateOp of Tensor.t
  | Linear of Tensor.t

module type Op = sig
  type t = op_type

  val to_stats : t -> Stats.t
end
