include Device_intf

module type InterConnect = sig
  val name : string
  val bandwidth : float
  val num_links : int
  val link_bandwidth : int -> float
  val efficiency : float
  val hop_penalty : float
end

module type Link = sig
  (* connections are undirected *)
  type a
  type b
  type t = a * b

  include InterConnect

  val make : a -> b -> t
end

module type IntraLink = sig
  include Link

  type a := (module Device)
  type b := (module Device)

  val make : a -> b -> t
end

module MakeIntra (I : InterConnect) : IntraLink = struct
  type a = (module Device)
  type b = (module Device)
  type t = a * b

  let name = I.name
  let bandwidth = I.bandwidth
  let num_links = I.num_links
  let link_bandwidth = I.link_bandwidth
  let efficiency = I.efficiency
  let hop_penalty = I.hop_penalty
  let make a b = a, b
end

module NvLinkIC : InterConnect = struct
  let name = "nvlink_v4"
  let bandwidth = 900.0 *. Int64.to_float Units.giga_b
  let num_links = 18

  let link_bandwidth low_level_links =
    bandwidth /. Int.to_float (num_links * low_level_links)
  ;;

  let efficiency = 0.7

  (* Switch latency *)
  (* NIC latency *)
  (* package mesh latency *)
  let hop_penalty = ((100. *. 2.) +. (100. *. 2.) +. 20.) *. Units.nano
end

module InfinibandIC : InterConnect = struct
  let name = "ConnectX-IB"
  let bandwidth = 400.0 *. Int64.to_float Units.giga_b
  let num_links = 18

  let link_bandwidth low_level_links =
    let res = bandwidth /. Int.to_float (num_links * low_level_links) in
    (* Printf.printf "IB:%f\n" res; *)
    res
  ;;

  let efficiency = 0.7
  let hop_penalty = ((100. *. 2.) +. (100. *. 2.) +. 20.) *. Units.nano
end

module NvLink = MakeIntra (NvLinkIC)
