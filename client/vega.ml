open! Js_of_ocaml

let vega_div = "#viz"

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
