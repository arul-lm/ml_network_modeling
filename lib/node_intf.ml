include Device_intf
include Link_intf
include Conn_intf
include Device_intf

module type Node = sig
  val device : (module Device)
  val dev_count : int
  val intra_link : (module IntraLink)
  val connections : Conn.t array array
  val devices : device_data array
  val intra_link_bw : int
end
