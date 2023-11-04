include Device_intf
include Switch_intf

module type InterConnect = sig
  val name : string
  val bandwidth : int
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

module type InterLink = sig
  include Link

  type a := (module Switch)
  type b := (module Device)

  val make : a -> b -> t
end

module MakeIntra (I : InterConnect) : IntraLink = struct
  type a = (module Device)
  type b = (module Device)
  type t = a * b

  let name = I.name
  let bandwidth = I.bandwidth
  let make a b = a, b
end

module MakeInter (I : InterConnect) : InterLink = struct
  type a = (module Switch)
  type b = (module Device)
  type t = a * b

  let name = I.name
  let bandwidth = I.bandwidth
  let make a b = a, b
end

module NvLinkIC : InterConnect = struct
  let name = "nvlink"
  let bandwidth = 900
end

module InfinibandIC : InterConnect = struct
  let name = "IB"
  let bandwidth = 400 (* connect x NIC *)
end

module NvLink = MakeIntra (NvLinkIC)
module Infiniband = MakeInter (InfinibandIC)
