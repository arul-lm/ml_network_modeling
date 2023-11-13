open! Ml_network_modeling

let () =
  let model = Transformers.opt13b in
  let nodes = Node_intf.make_nodes 8 in
  let wl = Transformer_wl.make ~batch_size:32 ~seq_len:512 ~mpar_factor:1 in
  Serialize.serialize_clos_dgx nodes model wl ~file_name:"data/clos.json"
;;
(* Serialize.serialize_clos_dgx nodes ~file_name:"clos.json" *)
