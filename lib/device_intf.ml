module type Device = sig
  type t

  val name : string
end

module H100 : Device = struct
  type t = int

  let name = "h100"
end
