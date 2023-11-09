open Op_intf
open Optimizer_state

type t = op_type

let tensor_stats t =
  let node = Tensor.node t in
  let device = Tensor.device t in
  let mem_used = Tensor.size t in
  let s = Stats.empty node device in
  Stats.add_mem s mem_used
;;

(* Stats.add_node_stats ~node ~device ~mem_used ~flops:0 *)

let optimizer_stats (module O : Optimizer) t =
  let node = Tensor.node t in
  let device = Tensor.device t in
  let mem_used =
    O.mem_used ~param_count:(Tensor.numel t + Tensor.numel t) |> Int.to_float
  in
  let s = Stats.empty node device in
  Stats.add_mem s mem_used
;;

let rec load_weight = function
  | Create t -> tensor_stats t
  | Linear (w, b) | LayerNorm (w, b) -> Stats.(tensor_stats w + tensor_stats b)
  | QKV wop -> Stats.(load_weight wop * 3.)
;;

let rec load_optimizer_states (module O : Optimizer) op =
  let calc_opt_stats = optimizer_stats (module O) in
  match op with
  | Create t -> calc_opt_stats t
  | Linear (w, b) | LayerNorm (w, b) -> Stats.(calc_opt_stats w + calc_opt_stats b)
  | QKV wop -> Stats.(load_optimizer_states (module O) wop * 3.)
;;

let matmul (module D : Device_intf.Device) x w =
  (* x can have multiple dimensions. w can have only two dimensions with overlapping dim as the first dim *)
  let node = Tensor.node x in
  let device = Tensor.device x in
  let dtype = Tensor.dtype x in
  let (module DT) = dtype in
  assert (Tensor.node w = Tensor.node x);
  assert (Tensor.device w = Tensor.device x);
  assert (Tensor.dtype w = Tensor.dtype x);
  assert (List.length (Tensor.shape w) = 2);
  let open Base in
  let kx = List.last_exn (Tensor.shape x) in
  let kw = List.hd_exn (Tensor.shape w) in
  let xs = List.drop_last_exn (Tensor.shape x) in
  let ys = List.tl_exn (Tensor.shape w) in
  let out_shape = xs @ ys in
  assert (kx = kw);
  let out = Tensor.make ~node ~device ~dtype out_shape |> Option.value_exn in
  let s = tensor_stats out in
  let flops = List.fold (kx :: out_shape) ~init:1 ~f:Int.( * ) in
  let c_lat = Int.to_float flops /. (D.tf32_tflops *. Int.to_float DT.nbytes) in
  Stdlib.Printf.printf "%d|%f\n" flops c_lat;
  out, Stats.add_flops s flops
;;

let rec w_forward d op x =
  match op with
  | Create t -> t, Stats.empty (Tensor.node t) (Tensor.device t)
  | Linear (w, _b) -> matmul d x w
  | LayerNorm (_w, _b) -> x, tensor_stats x
  | QKV wop ->
    let o, s = w_forward d wop x in
    o, Stats.(s * 3.)
;;

let bmm d node device dtype ?(transpose = true) x y =
  (* bxsxe, bxsxe -> bxsxe, bxexs *)
  let shape = Tensor.shape y in
  let ndims = List.length shape in
  let xs = Base.List.drop shape (ndims - 2) in
  let ys = if transpose then List.rev xs else xs in
  let k_t = Tensor.make ~node ~device ~dtype ys |> Option.get in
  (* QK^T *)
  let out, s = matmul d x k_t in
  out, s
;;

let no_param_forward d op x =
  let node = Tensor.node x in
  let device = Tensor.device x in
  let dtype = Tensor.dtype x in
  match op with
  | QK (_, _) -> bmm d node device dtype x x
  | AV y -> bmm d node device dtype ~transpose:false x y
  | Softmax (_, _) -> x, tensor_stats x
;;

let forward d op x =
  match op with
  | WeightOp op -> w_forward d op x
  | NoParamOp op -> no_param_forward d op x
;;

let is_weight_op = function
  | WeightOp o -> Some o
  | _ -> None
;;

let ( @ ) o = WeightOp o
let ( & ) o = NoParamOp o

let w_to_string = function
  | Create _ -> "create"
  | Linear (_, _) -> "linear"
  | LayerNorm (_, _) -> "lnorm"
  | QKV _ -> "qkv"
;;

let no_p_to_string = function
  | AV _ -> "AV"
  | QK (_, _) -> "QK"
  | Softmax (_, _) -> "Softmax"
;;

let to_string = function
  | WeightOp o -> w_to_string o
  | NoParamOp o -> no_p_to_string o
;;
