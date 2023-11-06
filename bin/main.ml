open Ml_network_modeling

let () =
  let nodes = Node_intf.make_dgx 4 in
  Serialize.serialize_clos_dgx nodes ~file_name:"clos.json"
;;
