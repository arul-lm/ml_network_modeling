include Node_intf
include Device_intf

type 'a switch_data = { id : int }

module type Switch = sig
  type t

  val name : string
  val max_ports : int
end

module type RailSwitch = sig
  include Switch with type t := (module Node) node_data
end

module Rail : RailSwitch = struct
  let name = "rail"
  let max_ports = 32
end

type 'a rail_data = { id : int }

let make_rails n =
  let result : (module RailSwitch) rail_data array = Array.init n (fun id -> { id }) in
  result
;;
(* module Spine : Switch = struct *)
(*   type a = (module Switch) *)
(*   type 'a t = (module Switch) switch_data *)
(*   let name = "spine" *)
(*   let max_ports = 32 *)
(*   (\* Not all switches will be filled. Use list. *\) *)
(*   let nodes_ref = ref [] *)

(*   let add_link n = *)
(*     let id = List.length !nodes_ref in *)
(*     if id < (max_ports - 1) then *)
(*       (nodes_ref := n :: !nodes_ref; Some ({id})) *)
(*     else *)
(*       None     *)
(* end *)
