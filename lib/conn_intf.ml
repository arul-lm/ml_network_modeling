module type Connection = sig
  type t

  val make : int * int -> int -> t
  val to_string : t -> string
end
