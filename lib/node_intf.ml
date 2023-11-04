include Device_intf
include Link_intf
include Conn_intf
include Device_intf
include Switch_intf

module type Node = sig
  val device : (module Device)
  val switch : (module Switch)
  val dev_count : int
  val intra_link : (module IntraLink)
  val inter_link : (module InterLink)

  val intra_connections
    : ((module Device) device_data, (module Device) device_data) Conn.t array array

  val inter_connections
    : ((module Device) device_data, (module Switch) switch_data) Conn.t array array

  val devices : (module Device) device_data array
  val switches : (module Switch) switch_data array
end
