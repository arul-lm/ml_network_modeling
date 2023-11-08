open Op_intf

type t = op_type

let tensor_stats t =
  let node = Tensor.node t in
  let device = Tensor.device t in
  let mem_used = Tensor.size t in
  Stats.add_node_stats ~node ~device ~mem_used
;;

let load_weight = function
  | Create t -> tensor_stats t
  | Linear (w, b) | LayerNorm (w, b) -> Stats.(tensor_stats w + tensor_stats b)
;;

let is_weight_op = function
  | WeightOp o -> Some o
  | _ -> None
;;

let ( @ ) o = WeightOp o
let ( & ) o = NoParamOp o

(* let forward = function *)
(*   |  *)
