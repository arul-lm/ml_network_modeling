open Ml_network_modeling

let () =
  let (module N) = (module Node.DGX : Node_intf.Node) in
  Serialize.serialize_links (module N) ~file_name:"links.json"
;;
