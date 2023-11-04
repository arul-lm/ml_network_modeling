open Ml_network_modeling

let () =
  let conn = Node.DGX.connections.(0).(0) in
  Printf.printf "%s\n" (Conn.to_string conn)
;;
