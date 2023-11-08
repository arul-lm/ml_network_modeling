module type Workload = sig
  type t

  val batch_size : t -> int
end

module type Transformer_WL = sig
  type t =
    { batch_size : int
    ; seq_len : int
    }
  [@@deriving make]

  include Workload with type t := t

  val seq_len : t -> int
end
