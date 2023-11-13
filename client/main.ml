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
  open Ml_network_modeling

  type t = Nvidia_DGX_Fat_Tree [@@deriving sexp_of, typed_variants]

  let label_for_variant = `Inferred
  let initial_choice = `Empty

  let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t = function
    | Nvidia_DGX_Fat_Tree -> Bonsai.const (Form.return ())
  ;;

  let to_l2 = function
    | Nvidia_DGX_Fat_Tree -> (module Level2_intf.Clos : Level2_intf.Level2)
  ;;
end

let topo_form = Form.Typed.Variant.make (module Topo)
let model_form = Form.Typed.Variant.make (module Model)

let component =
  let%map.Computation dyn = topo_form
  and model = model_form in
  let handle_click _e =
    let topo = Form.value_or_default dyn ~default:Topo.Nvidia_DGX_Fat_Tree in
    let (module L2) = Topo.to_l2 topo in
    Effect.print_s (Sexplib0.Sexp.Atom "loading graph...")
  in
  let graph_btn =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click handle_click ]
      [ Vdom.Node.Text "show graph" ]
  in
  Vdom.Node.div
    [ Form.View.to_vdom (Form.view (Form.label "Select Topology" dyn))
    ; Form.View.to_vdom (Form.view (Form.label "Select Model" model))
    ; graph_btn
    ]
;;

let () = Start.start ~bind_to_element_with_id:"app" component
