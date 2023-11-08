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

let matmul node device dtype (m, _k, n) =
  Tensor.make ~node ~device ~dtype [ m; n ] |> Option.get
;;

let matmul_prep w x =
  let node = Tensor.node x in
  let device = Tensor.device x in
  let dtype = Tensor.dtype x in
  assert (Tensor.node w = Tensor.node x);
  assert (Tensor.device w = Tensor.device x);
  assert (Tensor.dtype w = Tensor.dtype x);
  let kx = Base.List.last_exn (Tensor.shape x) in
  let kw = Base.List.last_exn (Tensor.shape w) in
  let m = Tensor.numel x / kx in
  let n = Tensor.numel w / kw in
  assert (kx = kw);
  let out = matmul node device dtype (m, kx, n) in
  out, tensor_stats out
;;

let w_forward op x =
  match op with
  | Create t -> t, Stats.empty (Tensor.node t) (Tensor.device t)
  | Linear (w, _b) -> matmul_prep w x
  | LayerNorm (_w, _b) -> x, tensor_stats x
;;

let no_param_forward op x =
  let node = Tensor.node x in
  let device = Tensor.device x in
  let dtype = Tensor.dtype x in
  match op with
  | QK (_, _) ->
    (* Tranpose K *)
    let shape = Tensor.shape x in
    assert (List.length shape = 3);
    let b = List.hd shape in
    let mk = List.tl shape in
    let km = List.rev mk in
    let k_t = Tensor.make ~node ~device ~dtype (b :: km) |> Option.get in
    let q_t = x in
    matmul_prep q_t k_t
  | Softmax (_, _) -> x, tensor_stats x
;;

let forward op x =
  match op with
  | WeightOp op -> w_forward op x
  | NoParamOp op -> no_param_forward op x
;;

let is_weight_op = function
  | WeightOp o -> Some o
  | _ -> None
;;

let ( @ ) o = WeightOp o
let ( & ) o = NoParamOp o
