open Tensor_intf

module type Optimizer = sig
  val dtype : (module Dtype)
  val mem_used : param_count:int -> int
end

let mem_used (dtype : (module Dtype)) ~param_count =
  let (module D) = dtype in
  param_count * D.nbytes
;;

module Adam : Optimizer = struct
  let dtype = (module FP32 : Dtype)

  (* fp32 copy of parameters, momentum, variance *)
  let mem_used ~param_count = 3 * mem_used dtype ~param_count
end

module SGD : Optimizer = struct
  let dtype = (module FP32 : Dtype)

  (* fp32 copy *)
  let mem_used = mem_used dtype
end
