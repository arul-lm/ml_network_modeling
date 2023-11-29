open! Ml_network_modeling

let () =
  let model = Transformers.opt13b in
  let nodes = Node_intf.make_nodes 10 in
  let wl = Transformer_wl.make ~batch_size:40 ~seq_len:512 ~mpar_factor:5 in
  let _ = Serialize.serialize_clos_dgx nodes model wl ~file_name:"data/clos.json" in
  let _ =
    Serialize.serialize_comm_time nodes model wl ~file_path:(Some "data/comms.json")
  in
  ()
;;
(* Serialize.serialize_clos_dgx nodes ~file_name:"clos.json" *)
