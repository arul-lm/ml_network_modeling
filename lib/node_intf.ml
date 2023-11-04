include Device_intf
include Link_intf
include Conn_intf

type device_data = { id : int }

module type Node = sig
  val device : (module Device)
  val dev_count : int
  val intra_link : (module IntraLink)
  val connections : Conn.t array array
  val devices : device_data array
end
