type conn_type = [ `AllToAll ]

module type Connection = sig
  type ('a, 'b) t

  val make : int * int -> 'a * 'b -> int -> ('a, 'b) t
  val to_string : ('a, 'b) t -> string

  val connections
    :  ?x_begin:int
    -> ?y_begin:int
    -> 'a array
    -> 'b array
    -> conn_type:conn_type
    -> ('a, 'b) t array array

  val conn_pair : ('a, 'b) t -> int * int
  val link_id : ('a, 'b) t -> int
  val endpoints : ('a, 'b) t -> 'a * 'b
end
