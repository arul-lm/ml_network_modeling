open! Device_intf
open Tensor_intf
open Node_intf

(* Tensors assigned to (node, device) *)
(* Keep track of the id *)
(* Collect list of tensors - ops *)
(* Allocate *)
(* Get updated stats - stats *)
type t =
  { shape : int list
  ; node : (module Node) node_data
  ; device : (module Device) device_data
  ; dtype : (module Dtype)
  }

let node t = t.node
let device t = t.device
let dtype t = t.dtype
let shape t = t.shape

let size t =
  let (module D) = t.dtype in
  Base.List.fold_left t.shape ~init:1 ~f:(fun acc s -> acc * s) * D.nbytes |> Int.to_float
;;

let make shape ~node ~device ~dtype =
  (* All numbers are > 0 *)
  (* Size of list must be > 0 *)
  Some { shape; node; device; dtype }
;;