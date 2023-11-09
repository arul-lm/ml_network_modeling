open Optimizer_state
open Tensor_intf

(* FP32 Adam - https://github.com/facebookresearch/metaseq/blob/main/projects/OPT/chronicles/10_percent_update.md *)
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

(* https://huggingface.co/facebook/opt-6.7b/blob/main/config.json *)
let opt6_7b =
  Transformer.make
    ~embed_dim:4096
    ~num_heads:32
    ~num_layers:32
    ~w_dtype:(module BF16)
    ~optimizer:(module Adam)
    ~is_train:true
  |> Option.get
;;

(* https://huggingface.co/facebook/opt-13b/blob/main/config.json *)
let opt13b =
  Transformer.make
    ~embed_dim:5120
    ~num_heads:40
    ~num_layers:40
    ~w_dtype:(module BF16)
    ~optimizer:(module Adam)
    ~is_train:true
  |> Option.get
;;

(* https://huggingface.co/facebook/opt-30b/blob/main/config.json *)
let opt30b =
  Transformer.make
    ~embed_dim:7168
    ~num_heads:56
    ~num_layers:48
    ~w_dtype:(module BF16)
    ~optimizer:(module Adam)
    ~is_train:true
  |> Option.get
;;

let bert_large =
  Transformer.make
    ~embed_dim:1024
    ~num_heads:64
    ~num_layers:24
    ~w_dtype:(module BF16)
    ~optimizer:(module Adam)
    ~is_train:true
  |> Option.get
;;
