open! Device_intf
open Tensor_intf
open Node_intf

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
let numel t = Base.List.fold_left t.shape ~init:1 ~f:(fun acc s -> acc * s)

let size t =
  let (module D) = t.dtype in
  numel t * D.nbytes |> Int.to_float
;;

let make ~node ~device ~dtype shape =
  (* All numbers are > 0 *)
  (* Size of list must be > 0 *)
  Some { shape; node; device; dtype }
;;

let to_string t = Base.String.concat ~sep:"," (Base.List.map ~f:Int.to_string (shape t))
