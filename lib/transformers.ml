open Optimizer_state
open Tensor_intf

let opt175b =
  Transformer.make
    ~embed_dim:12288
    ~num_heads:96
    ~num_layers:96
    ~w_dtype:(module BF16)
    ~is_train:true
    ~optimizer:(module Adam)
  |> Option.get
;;

(* https://huggingface.co/facebook/opt-1.3b/blob/main/config.json *)
let opt1_3b =
  Transformer.make
    ~embed_dim:2048
    ~num_heads:32
    ~num_layers:24
    ~w_dtype:(module BF16)
    ~optimizer:(module Adam)
    ~is_train:true
  |> Option.get
;;
