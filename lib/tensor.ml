open Device_intf
open Tensor_intf
    
type t = { shape: int list; device: (module Device) device_data; dtype: (module Dtype)}    

let make shape ~device ~dtype = {shape; device; dtype}

let device t = t.device

let dtype t = t.dtype

let shape t = t.shape

let size t =
  let (module D) = t.dtype in
  Base.List.fold_left t.shape ~init:1 ~f:(fun acc s -> acc * s) * D.nbytes
