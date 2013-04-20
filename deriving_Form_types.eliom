{shared{

open Deriving_Form_utils
open Deriving_Form_base

type ('a, 'param_names, 'template_data) widget =
  param_names:'param_names or_display ->
  ?value:'a default_constant_value ->
  ?a:Html5_types.div_attrib Eliom_content.Html5.F.attrib list ->
  template_data:'template_data ->
  unit ->
  form_content

let atomic_template : (_, _, _) Template.t -> (_, _, _) widget -> (_, _, _) Template.t =
  fun template widget ->
    Template.template
      (fun ~is_outmost ?submit ~config ~template_data ~param_names ~component_renderings () ->
        assert (component_renderings = []);
        Pre_local_config.bind config
          (fun ?label ?annotation ?value ?a () ->
            let content = widget ~param_names ?value ?a ?template_data () in
            if is_outmost then
              let default_constant = option_map ~f:default_constant value in
              let component_renderings = [
                { Component_rendering.
                  label; content; annotation; a; default_constant;
                  selector=None }
              ] in
              template
                (Template.arguments
                   ~is_outmost ?submit ~config ~template_data ~param_names ~component_renderings ())
            else
              content))


module type Atomic_options = sig
  type a
  type param_names
  include Template_data with type a := a
  val default_widget : (a, param_names, template_data) widget
  val params : string -> (a, [`WithoutSuffix], param_names) Eliom_parameter.params_type
end

module Make_atomic :
  functor (Atomic_options : Atomic_options) -> sig
    module Atomic_options : Atomic_options with
      type a = Atomic_options.a and
      type param_names = Atomic_options.param_names and
      type template_data = Atomic_options.template_data and
      type 'res template_data_fun = 'res Atomic_options.template_data_fun
    include Form with
      type a = Atomic_options.a and
      type repr = Atomic_options.a and
      type param_names = Atomic_options.param_names and
      type template_data = Atomic_options.template_data and
      type 'res template_data_fun = 'res Atomic_options.template_data_fun and
      type deep_config = unit and
      type config =
        ( Atomic_options.a, Atomic_options.param_names,
          Atomic_options.template_data, unit
        ) config' and
      type ('arg, 'res) opt_component_configs_fun = 'arg -> 'res
  end =
  functor (Atomic_options : Atomic_options) -> struct
    module Atomic_options = Atomic_options
    include Make_base
      (struct
        include Atomic_options
        let params' prefix = prefix, params prefix
        let component_names = []
        let default_deep_config = ()
        let opt_component_configs_fun k x = k () x
        type ('arg, 'res) opt_component_configs_fun = 'arg -> 'res
        type deep_config = unit
        type repr = a
        let of_repr x = x
        let to_repr x = x
        let default_template = atomic_template default_template default_widget
       end)
    let pre_render is_outmost submit param_names {local; deep=()} =
      let { Local_config.pre; template; template_data } = local in
      let template = option_get ~default:default_template template in
      let template_data =
        let default () =
          let value = option_map ~f:default_constant_get pre.Local_config.value in
          apply_template_data_fun (pre_template_data ~value identity)
        in
        option_get' ~default template_data
      in
      template
        (Template.arguments ~is_outmost ?submit ~config:pre
           ~template_data ~param_names ~component_renderings:[] ())
    let display = pre_display pre_render
    let content = pre_content pre_render
  end

let form_string_default_widget can_be_empty : (string,_,_) widget =
  let open Eliom_content.Html5.F in
  fun ~param_names ?value ?(a=[]) ~template_data:opt_pattern () ->
    let not_required_class_maybe, required_maybe =
      if can_be_empty then
        [component_not_required_class], []
      else
        [], [a_required `Required]
    in
    let pattern_maybe =
      option_get ~default:[]
        (option_map
           ~f:(fun p -> [a_pattern p])
           opt_pattern)
    in
    let a =
      let a = (a :> Html5_types.input_attrib Eliom_content.Html5.attrib list) in
      a_class not_required_class_maybe :: required_maybe @ pattern_maybe @ a
    in
    let hidden, value' = hidden_value value in
    match param_names with
    | `Param_names (_, param_names) ->
      let a = if hidden then a_hidden `Hidden :: a else a in
      [
        string_input ~a ~name:param_names ?value:value' ~input_type:`Text ();
        input_marker;
      ]
    | `Display ->
      if not hidden then
        option_get_map ~default:[] ~f:(fun x -> list_singleton (pcdata x)) value'
      else []

module Form_string =
  Make_atomic
    (struct
      type a = string
      type param_names = [`One of string] Eliom_parameter.param_name
      type template_data = string option
      type 'res template_data_fun = ?required_pattern:string -> unit -> 'res
      let pre_template_data ~value:_ k ?required_pattern () = k required_pattern
      let apply_template_data_fun (f : _ template_data_fun) = f ()
      let params = Eliom_parameter.string
      let default_widget = form_string_default_widget false
     end)

module Form_int =
  Make_atomic
    (struct
      type a = int
      type param_names = [`One of int] Eliom_parameter.param_name
      let params = Eliom_parameter.int
      type template_data = unit
      type 'res template_data_fun = 'res
      let pre_template_data ~value:_ k = k ()
      let apply_template_data_fun (f : _ template_data_fun) = f
      let default_widget : (_,_,_) widget =
        let open Eliom_content.Html5.F in
        fun ~param_names ?value ?(a=[]) ~template_data:() () ->
          let a = (a :> Html5_types.input_attrib Eliom_content.Html5.F.attrib list) in
          let hidden, value = hidden_value value in
          match param_names with
          | `Param_names (_, param_names) ->
            let a =
              a_required `Required ::
                if hidden then a_hidden `Hidden :: a else a
            in [
              int_input ~a ~name:param_names ?value ~input_type:`Number ();
              input_marker;
            ]
          | `Display ->
            if not hidden then
              option_get_map ~default:[]
                ~f:(fun x -> list_singleton (pcdata (string_of_int x)))
                value
            else []
      let default_template = atomic_template default_template default_widget
     end)

let int_widget :
    (?a:Html5_types.input_attrib Eliom_content_core.Html5.attrib list ->
     input_type:[> `Number] ->
     ?name:[< 'int Eliom_parameter.setoneradio ] Eliom_parameter.param_name ->
     ?value:'int ->
     unit -> [> Html5_types.input ] Eliom_content_core.Html5.elt) ->
    (?a:Html5_types.select_attrib Eliom_content_core.Html5.attrib list ->
     ?required:pcdata ->
     name:[< `One of 'int ] Eliom_parameter.param_name ->
     'int Eliom_content.Html5.F.select_opt ->
     'int Eliom_content.Html5.F.select_opt list ->
     [> Html5_types.select ] Eliom_content_core.Html5.elt) ->
    _ =
  let open Eliom_content.Html5.F in
  fun input select int_to_string ->
    fun ~param_names ?value ?(a=[]) ~template_data:values_opt () ->
    let required_label = "please select" in
    let hidden, value' = hidden_value value in
    match param_names with
      | `Param_names (_, param_names) -> begin
        match values_opt with
          | None ->
            let a = (a :> Html5_types.input_attrib Eliom_content.Html5.F.attrib list) in
            let a = a_required `Required :: if hidden then a_hidden `Hidden :: a else a in [
              input ~a ~name:param_names ?value:value' ~input_type:`Number ();
              input_marker;
            ]
          | Some values ->
            if values <> [] then
              let a = (a :> Html5_types.select_attrib Eliom_content.Html5.F.attrib list) in
              let a = if hidden then a_hidden `Hidden :: a else a in
              let options =
                List.map
                  (function i, label, selected ->
                    Option ([], i, Some label, selected))
                  values
              in [
                select ~a ~required:(pcdata required_label)
                  ~name:param_names (List.hd options) (List.tl options);
                input_marker;
              ]
            else
              let a' = (a :> Html5_types.select_attrib Eliom_content.Html5.F.attrib list) in
              let open Eliom_content.Html5.F.Raw in
              [ select ~a:(a_required `Required :: a')
                  [option ~a:[a_value ""] (pcdata required_label)] ]
      end
      | `Display -> [pcdata (option_get ~default:"" (option_map ~f:int_to_string value'))]

module Form_int64 =
  Make_atomic
    (struct
      type a = int64
      type param_names = [`One of int64] Eliom_parameter.param_name
      let params = Eliom_parameter.int64
      type template_data = (int64 * pcdata * bool) list option
      type 'res template_data_fun = ?from_list:(int64 * pcdata * bool) list -> unit -> 'res
      let pre_template_data ~value:_ k ?from_list () = k from_list
      let apply_template_data_fun (f : _ template_data_fun) = f ()
      let default_widget =
        let open Eliom_content.Html5.F in
        int_widget int64_input int64_select Int64.to_string
     end)

module Form_int32 =
  Make_atomic
    (struct
      type a = int32
      type param_names = [`One of int32] Eliom_parameter.param_name
      let params = Eliom_parameter.int32
      type template_data = (int32 * pcdata * bool) list option
      type 'res template_data_fun = ?from_list:(int32 * pcdata * bool) list -> unit -> 'res
      let pre_template_data ~value:_ k ?from_list () = k from_list
      let apply_template_data_fun (f : _ template_data_fun) = f ()
      let default_widget =
        let open Eliom_content.Html5.F in
        int_widget int32_input int32_select Int32.to_string
     end)

module Form_unit =
  Make_atomic
    (struct
      type a = unit
      type param_names = unit
      include Template_data_unit (struct type t = a end)
      let params _ = Eliom_parameter.unit
      let default_widget =
        fun ~param_names:_ ?value:_ ?a:_ ~template_data:() () ->
          []
     end)

(******************************************************************************)
type string_or_empty = string
module Form_string_or_empty =
  Make_atomic
    (struct
      include Form_string.Atomic_options
      let default_widget = form_string_default_widget true
     end)
}}
