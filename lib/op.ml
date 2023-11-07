open Op_intf

type t = op_type

let to_stats = function
  | CreateOp t | Linear t ->
    let node = Tensor.node t in
    let device = Tensor.device t in
    let mem_used = Tensor.size t in
    Stats.add_node_stats ~node ~device ~mem_used
;;
