open Ml_network_modeling

let () =
  let nodes = Node_intf.make_dgx 2 in
  Serialize.serialize_level1 nodes ~file_name:"level1.json"
;;
