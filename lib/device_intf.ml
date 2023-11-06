module type Device = sig
  val name : string
  val memory : float
end

module H100 : Device = struct
  let name = "h100"
  let memory = 80.0
end

(* Instance specific info *)
type 'a device_data =
  { id : int
  ; mem_used : float
  ; mem_cap : float
  }

let make_h100 n =
  let result : (module Device) device_data array =
    Array.init n (fun id -> { id; mem_used = 0.; mem_cap = H100.memory })
  in
  result
;;
