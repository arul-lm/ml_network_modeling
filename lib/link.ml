include Link_intf

module MakeIntra (I : InterConnect) : IntraLink = struct
  type a = (module Device)
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

module NvLink = MakeIntra (NvLinkIC)
