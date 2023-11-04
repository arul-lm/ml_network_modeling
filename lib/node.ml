include Node_intf
include Device_intf

module DGX : Node = struct
  let dev_count = 8
  let intra_link = (module Link_intf.NvLink : Link_intf.IntraLink)
  let inter_link = (module Link_intf.Infiniband : Link_intf.InterLink)
  let device = (module Device_intf.H100 : Device_intf.Device)
  let devices = Device_intf.(Array.init dev_count (fun id -> { id }))
  let switch = (module Switch_intf.Rail : Switch_intf.Switch)
  let switches = Switch_intf.(Array.init 1 (fun id -> { id }))
  let intra_connections = Conn.connections devices devices ~conn_type:`AllToAll
  let inter_connections = Conn.connections devices switches ~conn_type:`AllToAll
end
