include Device_intf

let to_string (module D : Device) = "Device Name:" ^ D.name

let make_devices (module D : Device) ~count =
  let d = (module D : Device) in
  Base.List.init count ~f:(Base.Fn.const d)
;;
