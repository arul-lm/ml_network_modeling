type conn_type = [ `AllToAll ]

module type Connection = sig
  type t

  val make : int * int -> int -> t
  val to_string : t -> string
  val connections : Device_intf.device_data array -> conn_type:conn_type -> t array array
  val device_pair : t -> int * int
  val link_id : t -> int
end
