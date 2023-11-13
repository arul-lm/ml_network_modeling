open! Core
open! Bonsai_web
module Form = Bonsai_web_ui_form
module E = Form.Elements

module ModelParams = struct
  type t =
    { model_par : int
    ; data_par : int
    ; batch_size : int
    ; seq_len : int
    }
  [@@deriving sexp_of, typed_fields]

  let label_for_field = `Inferred

  let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
    | Model_par -> E.Textbox.int ()
    | Data_par -> E.Textbox.int ()
    | Batch_size -> E.Textbox.int ()
    | Seq_len -> E.Textbox.int ()
  ;;
end

let model_params_form = Form.Typed.Record.make (module ModelParams)

module Model = struct
  type t = Opt13b of ModelParams.t [@@deriving sexp_of, typed_variants]

  let label_for_variant = `Inferred
  let initial_choice = `First_constructor

  let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t = function
    | Opt13b -> model_params_form
  ;;
end

module Topo = struct
  type t = Nvidia_Fat_Tree of unit [@@deriving sexp_of, typed_variants]

  let label_for_variant = `Inferred
  let initial_choice = `First_constructor

  let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t = function
    | Nvidia_Fat_Tree -> Bonsai.const (Form.return ())
  ;;
end

let topo_form = Form.Typed.Variant.make (module Topo)
let model_form = Form.Typed.Variant.make (module Model)

let component =
  let%map.Computation dyn = topo_form
  and model = model_form in
  Vdom.Node.div
    [ Form.View.to_vdom (Form.view (Form.label "Select Topology" dyn))
    ; Form.View.to_vdom (Form.view (Form.label "Select Model" model))
    ]
;;

let () = Start.start ~bind_to_element_with_id:"app" component
