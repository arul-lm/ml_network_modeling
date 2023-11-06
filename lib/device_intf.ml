module type Device = sig
  val name : string
  val memory : float
end

module H100 : Device = struct
  let name = "h100"
  let memory = 80.0 *. Int.to_float Units.giga_b
end

(* Instance specific info *)
type 'a device_data = { id : int }

let make_h100 n =
  let result : (module Device) device_data array = Array.init n (fun id -> { id }) in
  result
;;
