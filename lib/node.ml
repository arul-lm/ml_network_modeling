include Node_intf
include Device_intf

module DGX : Node = struct
  let dev_count = 8
  let intra_link = (module Link_intf.NvLink : Link_intf.IntraLink)
  let device = (module Device_intf.H100 : Device_intf.Device)

  let devices =
    let ds = Array.make dev_count { id = -1 } in
    Array.mapi_inplace (fun i _ -> { id = i }) ds;
    ds
  ;;

  let connections = Conn.connections devices ~conn_type:`AllToAll

  let intra_link_bw = Link_intf.NvLink.bandwidth
end
