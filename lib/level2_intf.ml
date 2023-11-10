open Level1_intf
open Op_intf

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
  val handle_comm : comm_op -> float
end

module Clos : Level2 = struct
  let l1 = (module DGX_L1 : Level1)
  let name = "clos"
  let switch = (module Spine : SpineSwitch)
  let switch_count = 1
  let switches = make_spines 1
  let inter_connections = Conn.connections switches DGX_L1.switches ~conn_type:`AllToAll
  let inter_link = (module Infiniband : InterLink)

  let handle_comm = function
    | Op_intf.AllReduce (mpar, nd, t) ->
      assert (mpar mod nd = 0);
      let size = Tensor.size t in
      let (module L1) = l1 in
      let (module N) = L1.node in
      let (module IN) = N.intra_link in
      let reduce_scatter_1d shard dim (module IN : InterConnect) ~low_lvl_count =
        let num_steps = Int.to_float (dim - 1) in
        (* bxsx(e/d) *)
        let size_per_step = shard /. Int.to_float dim in
        let send_vol = size_per_step in
        let recv_vol = size_per_step in
        let link_bw = IN.link_bandwidth low_lvl_count in
        let time_per_step =
          (send_vol +. recv_vol) /. (link_bw *. Int.to_float IN.num_links)
        in
        let comm_time = time_per_step *. num_steps in
        let comm_time = comm_time +. (IN.hop_penalty *. num_steps) in
        let comm_time = comm_time /. IN.efficiency in
        comm_time
      in
      let all_reduce_1d shard dim =
        let comm = reduce_scatter_1d shard dim (module Link_intf.NvLinkIC) ~low_lvl_count:1 in
        comm *. 2.
      in
      let all_reduce_2d shard dim1 dim2 =
        let d_shard = shard /. Int.to_float dim1 in
        let intra_comm = all_reduce_1d d_shard dim2 in
        let inter_comm =
          let comm = reduce_scatter_1d d_shard dim1 (module Link_intf.InfinibandIC) ~low_lvl_count:N.dev_count in
          comm *. 2.
        in
        intra_comm +. inter_comm
      in
      if mpar > nd
      then all_reduce_2d size (mpar / nd) nd (* mpar is an integer multiple of nd *)
      else all_reduce_1d size mpar
  ;;
end
