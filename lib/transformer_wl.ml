type t =
  { batch_size : int
  ; seq_len : int
  ; mpar_factor : int
  }
[@@deriving make]

let batch_size t = t.batch_size
let seq_len t = t.seq_len
let mpar_factor t = t.mpar_factor
