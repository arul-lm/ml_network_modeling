open Op_intf

type t = op_type

let tensor_stats t =
  let node = Tensor.node t in
  let device = Tensor.device t in
  let mem_used = Tensor.size t in
  Stats.add_node_stats ~node ~device ~mem_used
;;

let load_op = function
  | CreateOp t -> tensor_stats t
  | Linear (w, b) | LayerNorm (w, b) -> Stats.(tensor_stats w + tensor_stats b)
  | QK (node, device) -> Stats.add_node_stats ~node ~device ~mem_used:0.
  | Softmax (node, device) -> Stats.add_node_stats ~node ~device ~mem_used:0.
;;
