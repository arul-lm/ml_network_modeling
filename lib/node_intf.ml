include Device_intf
include Link_intf
include Conn_intf
include Device_intf

module type Node = sig
  val device : (module Device)
  val name : string
  val dev_count : int
  val intra_link : (module IntraLink)

  val intra_connections
    : int * ((module Device) device_data, (module Device) device_data) Conn.t array array

  val devices : (module Device) device_data array
end

type 'a node_data = { id : int }

module DGX : Node = struct
  let name = "dgx"
  let dev_count = 8
  let intra_link = (module Link_intf.NvLink : Link_intf.IntraLink)
  let device = (module Device_intf.H100 : Device_intf.Device)
  let devices = make_h100 dev_count
  let intra_connections = Conn.connections devices devices ~conn_type:`AllToAll
end

let make_nodes n =
  let result : (module Node) node_data array = Array.init n (fun id -> { id }) in
  result
;;
