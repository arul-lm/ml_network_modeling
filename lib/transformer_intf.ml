module type Transformer = sig
  type t

  val embed_dim : t -> int
  val num_heads : t -> int
  val num_layers : t -> int
  val head_dim : t -> int
  val make : embed_dim:int -> num_heads:int -> num_layers:int -> t option
end
