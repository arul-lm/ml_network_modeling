open! Core
open! Bonsai_web
module Form = Bonsai_web_ui_form
module E = Form.Elements
open Ml_network_modeling

module ModelParams = struct
  type t =
    { node_count : int
    ; model_par : int
    ; batch_size : int
    ; seq_len : int
    }
  [@@deriving sexp_of, typed_fields]

  let label_for_field = `Inferred

  let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
    | Model_par -> E.Textbox.int ()
    | Batch_size -> E.Textbox.int ()
    | Seq_len -> E.Textbox.int ()
    | Node_count -> E.Textbox.int ()
  ;;

  let to_workload dev_count t =
    let mpar_factor = t.model_par / dev_count in
    Transformer_wl.make ~batch_size:t.batch_size ~seq_len:t.seq_len ~mpar_factor
  ;;

  let get_node_count t = t.node_count
end

let model_params_form = Form.Typed.Record.make (module ModelParams)

module Model = struct
  type t = Opt13b of ModelParams.t [@@deriving sexp_of, typed_variants]

  let label_for_variant = `Inferred
  let initial_choice = `First_constructor

  let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t = function
    | Opt13b -> model_params_form
  ;;

  let get_model = function
    | Opt13b _ -> Transformers.opt13b
  ;;

  let to_workload dev_count = function
    | Opt13b mp -> ModelParams.to_workload dev_count mp
  ;;

  let get_node_count = function
    | Opt13b mp -> ModelParams.get_node_count mp
  ;;
end

module Topo = struct
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

let default_model_params =
  ModelParams.{ node_count = 10; model_par = 8; batch_size = 32; seq_len = 512 }
;;

let component =
  let%map.Computation dyn = topo_form
  and model = model_form in
  let handle_click _e =
    let topo = Form.value_or_default dyn ~default:Topo.Nvidia_DGX_Fat_Tree in
    let mdl = Form.value_or_default model ~default:(Model.Opt13b default_model_params) in
    let (module L2) = Topo.to_l2 topo in
    let (module L1) = L2.l1 in
    let (module N) = L1.node in
    let node_count = Model.get_node_count mdl in
    let nodes = Node_intf.make_nodes node_count in
    let model = Model.get_model mdl in
    let wl = Model.to_workload N.dev_count mdl in
    Serialize.serialize_clos_dgx nodes model wl ~file_name:"clos.json";
    (* let mpar = Model.get_model_par mdl |> Int.to_string in *)
    Effect.print_s (Sexplib0.Sexp.Atom ("loading graph..." ^ L2.name))
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
