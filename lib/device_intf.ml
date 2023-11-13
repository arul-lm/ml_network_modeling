module type Device = sig
  val name : string
  val memory : float
  val mem_name : string
  val tf32_tflops : float
  val mem_bw : float
  val mem_util : float
  val mvp_util : float
end

module H100 : Device = struct
  (* https://resources.nvidia.com/en-us-tensor-core *)
  let name = "h100sxm5"
  let mem_name = "hbm2e"
  let mem_per_stack = 16. *. Int64.to_float Units.giga_b
  let num_stacks = 5
  let mem_bw = 3352. *. Int64.to_float Units.giga_b

  (* Page 18 *)
  let memory = mem_per_stack *. Int.to_float num_stacks
  let tf32_tflops = 494.7 *. Int64.to_float Units.tera_b
  let mem_util = 0.75
  let mvp_util = 0.5
end

(* Instance specific info *)
type 'a device_data = { id : int }

let make_h100 n =
  let result : (module Device) device_data array = Array.init n (fun id -> { id }) in
  result
;;
