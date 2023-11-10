open Op_intf
open Optimizer_state
open Device_intf
open Tensor_intf

type t = op_type

let tensor_stats t =
  let node = Tensor.node t in
  let device = Tensor.device t in
  let mem_used = Tensor.size t in
  let s = Stats.empty node device in
  Stats.add_mem s mem_used
;;

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

let matmul x w =
  (* x can have multiple dimensions. w can have only two dimensions with overlapping dim as the first dim *)
  let node = Tensor.node x in
  let device = Tensor.device x in
  let dtype = Tensor.dtype x in
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
  out, Stats.add_flops s flops
;;

let matmul_lat (module D : Device) (module DT : Dtype) flops m_reads =
  let c_lat =
    Int.to_float flops /. (D.tf32_tflops *. Int.to_float DT.nbytes *. D.mvp_util)
  in
  let m_reads = m_reads *. Int.to_float DT.nbytes in
  let m_tput = D.mem_bw *. D.mvp_util in
  let m_lat = m_reads /. m_tput in
  Float.max c_lat m_lat
;;

let matmul_wrapper (module D : Device) x y y' =
  let out, s = matmul x y' in
  let m_reads = Tensor.size x +. Tensor.size y in
  let lat = matmul_lat (module D) (Tensor.dtype x) (Stats.flops s) m_reads in
  let s = Stats.add_lat s lat in
  out, s
;;

let rec w_forward (module D : Device) op x =
  match op with
  | Create t -> t, Stats.empty (Tensor.node t) (Tensor.device t)
  | Linear (w, _b) -> matmul_wrapper (module D) x w w
  | LayerNorm (_w, _b) -> x, tensor_stats x
  | QKV wop ->
    let o, s = w_forward (module D) wop x in
    o, Stats.(s * 3.)
;;

let bmm (module D : Device) node device dtype ?(transpose = true) x y =
  (* bxsxe, bxsxe -> bxsxe, bxexs *)
  let shape = Tensor.shape y in
  let ndims = List.length shape in
  (* bxsxe -> exs *)
  let xs = Base.List.drop shape (ndims - 2) in
  let ys = if transpose then List.rev xs else xs in
  let k_t = Tensor.make ~node ~device ~dtype ys |> Option.get in
  (* QK^T *)
  matmul_wrapper (module D) x y k_t
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

let is_compute_op = function
  | ComputeOp o -> Some o
  | _ -> None
;;

let ( @ ) o = ComputeOp (WeightOp o)
let ( & ) o = ComputeOp (NoParamOp o)

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

let comp_to_string = function
  | WeightOp o -> w_to_string o
  | NoParamOp o -> no_p_to_string o
;;

let comm_to_string = function
  | AllReduce _t -> "AllReduce"
;;

let to_string = function
  | ComputeOp o -> comp_to_string o
  | CommOp o -> comm_to_string o
;;
