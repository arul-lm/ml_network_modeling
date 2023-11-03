open Ml_network_modeling

let () =
  let h100 = (module Device_intf.H100 : Device_intf.Device) in
  let node = Device_utils.make_devices ~count:8 h100 in
  Base.List.iteri node ~f:(fun i d -> Printf.printf "%d|%s\n" i (Device_utils.to_string d))
;;
