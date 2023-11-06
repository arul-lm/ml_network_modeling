include Node_intf
include Device_intf

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

type 'a switch_data = { id : int }

let make_rails n =
  let result : (module RailSwitch) switch_data array = Array.init n (fun id -> { id }) in
  result
;;

module type SpineSwitch = sig
  include Switch with type t := (module RailSwitch) switch_data
end

module Spine : SpineSwitch = struct
  let name = "spine"
  let max_ports = 32
end

(* type 'a spine_data = { id : int } *)

let make_spines n =
  let result : (module SpineSwitch) switch_data array = Array.init n (fun id -> { id }) in
  result
;;
