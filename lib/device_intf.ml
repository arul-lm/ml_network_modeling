module type Device = sig
  val name : string
end

module H100 : Device = struct
  let name = "h100"
end

(* Instance specific info *)
type 'a device_data = { id : int (* ; device : (module Device) *) }
