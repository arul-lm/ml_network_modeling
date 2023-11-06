open Device_intf
open Tensor_intf

type t =
  { shape : int list
  ; device : (module Device) device_data
  ; dtype : (module Dtype)
  }

let device t = t.device
let dtype t = t.dtype
let shape t = t.shape

let size t =
  let (module D) = t.dtype in
  Base.List.fold_left t.shape ~init:1 ~f:(fun acc s -> acc * s) * D.nbytes |> Int.to_float
;;

let make shape ~device ~dtype =
  (* All numbers are > 0 *)
  (* Size of list must be > 0 *)
  let t = { shape; device; dtype } in
  let t_size = size t in
  let new_size = device.mem_used +. t_size in
  if new_size < device.mem_cap
  then (
    let device = { device with mem_used = new_size } in
    Some { shape; device; dtype })
  else None
;;
