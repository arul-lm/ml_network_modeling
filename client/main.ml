open! Core
open! Bonsai_web
module Form = Bonsai_web_ui_form
module E = Form.Elements

module Topo = struct
  type t = Text of string [@@deriving sexp_of, typed_variants]

  let to_string : type a. a Typed_variant.t -> string = function
    | Text -> "String"
  ;;

  let label_for_variant = `Computed to_string
  let initial_choice = `First_constructor

  let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t = function
    | Text -> E.Textbox.string ()
  ;;
end

let topo_form = Form.Typed.Variant.make (module Topo)

let component =
  let%map.Computation dyn = topo_form in
  Vdom.Node.div
    [ Vdom.Node.h1 [ Vdom.Node.text "Typed Variants" ]
    ; Form.View.to_vdom (Form.view (Form.label "dynamic value" dyn))
    ]
;;

let () = Start.start ~bind_to_element_with_id:"app" component
