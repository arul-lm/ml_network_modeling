include Device_intf

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

module type MAKEINTRALINK = functor (_ : InterConnect) -> IntraLink

(* module NvLink : IntraLink = struct *)
(*   let name = "nvlinkc2c" *)
(*   let bandwidth = 900 *)
(* end *)

(* module type InterLink = sig *)
(*   include Link *)

(*   type t = (module Node) * (module Node) *)
(* end *)
