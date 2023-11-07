type t =
  { embed_dim : int
  ; num_heads : int
  ; num_layers : int
  }

let embed_dim t = t.embed_dim
let num_heads t = t.num_heads
let num_layers t = t.num_layers
let head_dim t = t.embed_dim / t.num_heads

let make ~embed_dim ~num_heads ~num_layers =
  let t = { embed_dim; num_heads; num_layers } in
  let rem = embed_dim mod num_heads in
  if rem <> 0 then None else Some t
;;
