open Op_intf
open Optimizer_state

type t = op_type

let tensor_stats t =
  let node = Tensor.node t in
  let device = Tensor.device t in
  let mem_used = Tensor.size t in
  Stats.add_node_stats ~node ~device ~mem_used
;;

let optimizer_stats (module O : Optimizer) t =
  let node = Tensor.node t in
  let device = Tensor.device t in
  let mem_used =
    O.mem_used ~param_count:(Tensor.numel t + Tensor.numel t) |> Int.to_float
  in
  Stats.add_node_stats ~node ~device ~mem_used
;;

let load_weight = function
  | Create t -> tensor_stats t
  | Linear (w, b) | LayerNorm (w, b) -> Stats.(tensor_stats w + tensor_stats b)
;;

let load_optimizer_states (module O : Optimizer) op =
  let calc_opt_stats = optimizer_stats (module O) in
  match op with
  | Create t -> calc_opt_stats t
  | Linear (w, b) | LayerNorm (w, b) -> Stats.(calc_opt_stats w + calc_opt_stats b)
;;

let is_weight_op = function
  | WeightOp o -> Some o
  | _ -> None
;;

let ( @ ) o = WeightOp o
let ( & ) o = NoParamOp o
