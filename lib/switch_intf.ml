include Node_intf
include Device_intf

type 'a switch_data = { id : int }

(* module type InterLink = sig *)
(*   include Link *)

(*   type a := (module Switch) *)
(*   type b := (module Device) *)

(*   val make : a -> b -> t *)
(* end *)

(* module MakeInter (I : InterConnect) : InterLink = struct *)
(*   type a = (module Switch) *)
(*   type b = (module Device) *)
(*   type t = a * b *)

(*   let name = I.name *)
(*   let bandwidth = I.bandwidth *)
(*   let make a b = a, b *)
(* end *)
(* module Infiniband = MakeInter (InfinibandIC) *)

module type Switch = sig
  type t

  val name : string
  val max_ports : int

  (* val inter_link : (module InterLink) *)
  (* val add_node : (module Node) -> (module Node) node_data option *)
end

module type RailSwitch = sig
  include Switch

  (* Depending on rail id, pass appropriate device data *)
  val add_conn : node_data -> (module Device) device_data -> bool
end

module Rail : RailSwitch = struct
  type t = node_data

  let name = "rail"
  let max_ports = 32

  (* let inter_link = (module Link_intf.I) *)
  (* Not all switches will be filled. Use list. *)
  let connections = ref []

  let add_conn n d =
    let id = List.length !connections in
    if id < max_ports - 1
    then (
      connections := (n, d) :: !connections;
      true)
    else false
  ;;
end

type rail_data =
  { id : int
  ; rail : (module RailSwitch)
  }

let make_rails rail n = Array.init n (fun id -> { id; rail })
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
