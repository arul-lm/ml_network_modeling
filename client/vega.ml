open! Js_of_ocaml

let vega_div = "#viz"
let vega_obj_key = "VEGA_DEBUG"

let update_dataset ?(name = "main") values =
  let open Jv in
  let open Base in
  let v = find global vega_obj_key |> Option.value_exn in
  let view = find v "view" |> Option.value_exn in
  let vega = find global "vega" |> Option.value_exn in
  let cset = call vega "changeset" [||] in
  let rm = call cset "remove" [| get vega "truthy" |] in
  let ins = call rm "insert" [| values |] in
  let change = call view "change" [| of_string name; ins |] in
  let _ = call change "run" [||] in
  ()
;;

let json_parse s =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "JSON.parse") [| Js.Unsafe.inject (Js.string s) |]
;;

let vega_obj_key = "VEGA_DEBUG"
let await = Fun.flip Fut.await

let vega_embed json_spec =
  let vpromise = Jv.call Jv.global "vegaEmbed" [| Jv.of_string vega_div; json_spec |] in
  let attach_to_global view = Jv.set Jv.global vega_obj_key view in
  Fut.of_promise ~ok:attach_to_global vpromise |> await Result.get_ok;
  Brr.Console.(log [ str "vega_debug attached" ])
;;
