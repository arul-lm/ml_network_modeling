open Device_intf
open Tensor_intf
    
type t = { shape: int list; device: (module Device) device_data; dtype: (module Dtype)}    

let make shape ~device ~dtype = {shape; device; dtype}

let device t = t.device

let dtype t = t.dtype

let shape t = t.shape
