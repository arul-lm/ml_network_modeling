include Node_intf
include Device_intf

module DGX : Node = struct
  let dev_count = 8
  let intra_link = (module Link.NvLink : Link_intf.IntraLink)
  let device = (module Device_intf.H100 : Device_intf.Device)

  let devices =
    let ds = Array.make dev_count { id = -1 } in
    Array.mapi_inplace (fun i _ -> { id = i }) ds;
    ds
  ;;

  let connections =
    let link_id = ref 0 in
    (* all-to-all *)
    let make_conn { id = src_id } =
      let form_conn { id = dst_id } =
        if src_id = dst_id
        then None
        else (
          let conn = Conn.make (src_id, dst_id) !link_id in
          link_id := !link_id + 1;
          Some conn)
      in
      Base.Array.filter_map devices ~f:form_conn
    in
    let result = Array.map make_conn devices in
    result
  ;;
end
