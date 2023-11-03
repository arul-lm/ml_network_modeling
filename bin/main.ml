open Ml_network_modeling

let () =
  let h100 = (module Device_intf.H100 : Device_intf.Device) in
  Printf.printf "%s" (Device_utils.to_string h100)
