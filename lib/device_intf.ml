module type Device = sig
  val name : string
end

module H100 : Device = struct
  let name = "h100"
end

type device_data = { id : int }
