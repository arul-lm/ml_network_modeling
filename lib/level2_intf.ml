open Level1_intf

module type Level2 = sig
  val l1 : (module Level1)
  val name : string
  val switch : (module SpineSwitch)
  val switch_count : int
  val switches : (module SpineSwitch) switch_data array

  val inter_connections
    : int
      * ((module SpineSwitch) switch_data, (module RailSwitch) switch_data) Conn.t array
          array

  val inter_link : (module InterLink)
end

module Clos : Level2 = struct
  let l1 = (module DGX_L1 : Level1)
  let name = "clos"
  let switch = (module Spine : SpineSwitch)
  let switch_count = 1
  let switches = make_spines 1
  let inter_connections = Conn.connections switches DGX_L1.switches ~conn_type:`AllToAll
  let inter_link = (module Infiniband : InterLink)
end
