include Node_intf
include Switch_intf
include Conn_intf

module type InterLink = sig
  include Link

  type a := (module RailSwitch)
  type b := (module Device)

  val make : a -> b -> t
end

module type Level1 = sig
  val node : (module Node)
  val name : string
  val switch_count : int
  val switch : (module RailSwitch)
  val switches : (module RailSwitch) switch_data array
  val inter_link : (module InterLink)

  val inter_connections
    :  (module Node) node_data array
    -> int * ((module RailSwitch) switch_data, (module Node) node_data) Conn.t array array
end

module MakeInter (I : InterConnect) : InterLink = struct
  type a = (module RailSwitch)
  type b = (module Device)
  type t = a * b

  let name = I.name
  let bandwidth = I.bandwidth
  let make a b = a, b
end

module Infiniband = MakeInter (InfinibandIC)

module DGX_L1 : Level1 = struct
  let node = (module DGX : Node_intf.Node)
  let name = "dgx_l1"

  let switch_count =
    let (module N) = node in
    N.dev_count
  ;;

  let switch = (module Rail : Switch_intf.RailSwitch)
  let inter_link = (module Infiniband : InterLink)
  let switches = Switch_intf.make_rails switch_count
  let inter_connections nodes = Conn.connections switches nodes ~conn_type:`AllToAll
end
