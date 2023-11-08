open! Device_intf
open Node_intf

module type Dtype = sig
  val name : string
  val nbytes : int
end

module FP32 : Dtype = struct
  let name = "fp32"
  let nbytes = 4
end

module TF32 : Dtype = struct
  let name = "tf32"
  let nbytes = 2
end

module FP16 : Dtype = struct
  let name = "fp16"
  let nbytes = 2
end

module BF16 : Dtype = struct
  let name = "bf16"
  let nbytes = 2
end

module type Tensor = sig
  type t

  val make
    :  node:(module Node) node_data
    -> device:(module Device) device_data
    -> dtype:(module Dtype)
    -> int list
    -> t option

  val device : t -> (module Device) device_data
  val dtype : t -> (module Dtype)
  val shape : t -> int list
  val size : t -> float
  val node : t -> (module Node) node_data
  val numel : t -> int
end
