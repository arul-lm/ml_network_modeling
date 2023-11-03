val to_string : (module Device_intf.Device) -> string

val make_devices
  :  (module Device_intf.Device)
  -> count:int
  -> (module Device_intf.Device) list
