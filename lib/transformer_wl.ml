type t =
  { batch_size : int
  ; seq_len : int
  }
[@@deriving make]

let batch_size t = t.batch_size
let seq_len t = t.seq_len
