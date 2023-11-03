include Device_intf

let to_string (module D : Device) =
  "Device Name:" ^ D.name
