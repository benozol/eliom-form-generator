{shared{

open Deriving_Form_utils
open Deriving_Form_base

let required_label = "please select"

type ('a, 'param_names, 'template_data) widget =
  param_names:'param_names or_display ->
  ?value:'a default_constant ->
  ?a:Html5_types.div_attrib Eliom_content.Html5.F.attrib list ->
  template_data:'template_data ->
  unit ->
  form_content

let atomic_template : (_, _, _) Template.t -> (_, _, _) widget -> (_, _, _) Template.t =
  fun template widget ->
    Template.template
      (fun ~is_outmost ?submit ~config ~template_data ~param_names ~component_renderings () ->
        assert (component_renderings = []);
        let { Local_config.value; a; _ } = config in
        let content = widget ~param_names ?value ?a ?template_data () in
        if is_outmost then
          let component_renderings = [ {
            Component_rendering.content;
            surrounding = Pre_local_config.to_surrounding config
          } ] in
          template
            (Template.arguments
               ~is_outmost ?submit ~config ~template_data
               ~param_names ~component_renderings ())
        else
          content)


module type Atomic_options = sig
  type a
  type param_name
  include Template_data with type a := a
  val default_widget : (a, param_name, template_data) widget
  val params_type : string -> (a, [`WithoutSuffix], param_name) Eliom_parameter.params_type
end

module Make_atomic :
  functor (Atomic_options : Atomic_options) -> sig
    module Atomic_options : Atomic_options with
      type a = Atomic_options.a and
      type param_name = Atomic_options.param_name and
      type template_data = Atomic_options.template_data and
      type 'res template_data_fun = 'res Atomic_options.template_data_fun
    include Form with
      type a = Atomic_options.a and
      type raw_repr = Atomic_options.a and
      type raw_param_names = Atomic_options.param_name and
      type template_data = Atomic_options.template_data and
      type 'res template_data_fun = 'res Atomic_options.template_data_fun and
      type deep_config = unit and
      type config =
        ( Atomic_options.a, Atomic_options.param_name,
          Atomic_options.template_data, unit
        ) config' and
      type ('arg, 'res) opt_component_configs_fun = 'arg -> 'res
  end =
  functor (Atomic_options : Atomic_options) -> struct
    module Atomic_options = Atomic_options
    include Make_base
      (struct
        include Atomic_options
        type raw_param_names = param_name
        let params' prefix = prefix, params_type prefix
        let component_names = []
        let default_deep_config = ()
        let opt_component_configs_fun k x = k () x
        let apply_component_configs f x = f x
        type ('arg, 'res) opt_component_configs_fun = 'arg -> 'res
        type deep_config = unit
        type raw_repr = a
        let of_raw_repr x = x
        let to_raw_repr x = x
        let default_template = atomic_template default_template default_widget
       end)
    module Pre (Monad : Monad_with_template_data) = struct
    let pre_render is_outmost submit param_names ~config ~config_override =
      let open Local_config in
      let local = option_or_by_field config_override.local config.local in
      let template = option_get ~default:default_template local.template in
      Monad.bind
        (let default () =
           let value = option_map ~f:default_constant_get local.pre.value in
           let module Template_data = Monad.Template_data (Atomic_options) in
           Template_data.template_data ~value
         in
         option_get' ~default @
           option_map ~f:Monad.return local.template_data)
      @ fun template_data ->
        Monad.return @ template @
          Template.arguments ~is_outmost ?submit ~config:local.pre
            ~template_data ~param_names ~component_renderings:[] ()
    end
    let content =
      let module Pre = Pre (Identity_with_template_data) in
      pre_content Identity.map Pre.pre_render
    let content_lwt =
      let module Pre = Pre (Lwt_with_template_data) in
      pre_content Lwt.map Pre.pre_render
    let display =
      let module Pre = Pre (Identity_with_template_data) in
      pre_display Pre.pre_render
    let display_lwt =
      let module Pre = Pre (Lwt_with_template_data) in
      pre_display Pre.pre_render
  end

let form_string_default_widget can_be_empty : (string, _, _) widget =
  let open Eliom_content.Html5.F in
  fun ~param_names ?value ?(a=[]) ~template_data:opt_pattern_or_from_list () ->
    let not_required_class_maybe, required_maybe =
      if can_be_empty then
        [component_not_required_class], []
      else
        [], [a_required `Required]
    in
    let local_class_a = a_class not_required_class_maybe in
    let hidden, value' = hidden_value value in
    match param_names with
    | `Param_names (_, param_names) ->
      let a = if hidden then a_hidden `Hidden :: a else a in
      let input pattern_maybe =
        let a = local_class_a :: required_maybe @@ pattern_maybe @@ (a :> Html5_types.input_attrib Eliom_content.Html5.attrib list) in
        [
          string_input ~a ~name:param_names ?value:value' ~input_type:`Text ();
          input_marker;
        ]
      in
      (match opt_pattern_or_from_list with
        | None -> input []
        | Some (`Required_pattern required_pattern) ->
          input [a_pattern required_pattern]
        | Some (`From_list options) ->
          let a = local_class_a :: (a :> Html5_types.select_attrib Eliom_content.Html5.attrib list) in
          let mk_option (abb, label, selected) =
            Option ([], abb, Some label, selected)
          in
          (match options with
            | option :: options ->
               [
                string_select ~a ~name:param_names ~required:(pcdata required_label)
                  (mk_option option) (List.map mk_option options);
                input_marker;
              ]
            | [] ->
               [
                select ~a:(required_maybe @@ a) ~name:param_names
                  (mk_option ("", (pcdata required_label), false)) []
              ]))
    | `Display ->
      if not hidden then
        option_get_map ~default:[] ~f:(list_singleton -| pcdata)
          value'
      else []

module Form_string =
  Make_atomic
    (struct
      type a = string
      type param_name = [`One of string] Eliom_parameter.param_name
      type template_data = [`Required_pattern of string | `From_list of (string * pcdata * bool) list] option
      type 'res template_data_fun = template_data -> 'res
      let template_data ~value:_ = identity
      let template_data_lwt ~value:_ = Lwt.return
      let apply_template_data_fun f = f None
      let params_type = Eliom_parameter.string
      let default_widget = form_string_default_widget false
     end)

type text = string

let form_text_default_widget can_be_empty : (text, _, _) widget =
  let open Eliom_content.Html5.F in
  fun ~param_names ?value ?(a=[]) ~template_data:() () ->
    let not_required_class_maybe, required_maybe =
      if can_be_empty then
        [component_not_required_class], []
      else
        [], [a_required `Required]
    in
    let local_class_a = a_class not_required_class_maybe in
    let hidden, value' = hidden_value value in
    match param_names with
    | `Param_names (_, param_names) ->
      let a = if hidden then a_hidden `Hidden :: a else a in
      let a = local_class_a :: required_maybe @@ (a :> Html5_types.textarea_attrib Eliom_content.Html5.attrib list) in
      [
        textarea ~a ~name:param_names ?value:value' ();
        input_marker;
      ]
    | `Display ->
      if not hidden then
        option_get_map ~default:[] ~f:(list_singleton -| pcdata)
          value'
      else []


module Form_text =
  Make_atomic
    (struct
      type a = string
      type param_name = [`One of string] Eliom_parameter.param_name
      type template_data = unit
      type 'res template_data_fun = template_data -> 'res
      let template_data ~value:_ = identity
      let template_data_lwt ~value:_ = Lwt.return
      let apply_template_data_fun f = f ()
      let params_type = Eliom_parameter.string
      let default_widget = form_text_default_widget false
     end)

module Form_int =
  Make_atomic
    (struct
      type a = int
      type param_name = [`One of int] Eliom_parameter.param_name
      let params_type = Eliom_parameter.int
      type template_data = unit
      type 'res template_data_fun = 'res
      let template_data ~value:_ = ()
      let template_data_lwt ~value:_ = Lwt.return ()
      let apply_template_data_fun x = x
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
    let hidden, value' = hidden_value value in
    match param_names with
      | `Param_names (_, param_names) -> begin
        match values_opt with
          | None ->
            let a = (a :> Html5_types.input_attrib Eliom_content.Html5.F.attrib list) in
            let a = a_required `Required :: if hidden then a_hidden `Hidden :: a else a in
             [
              input ~a ~name:param_names ?value:value' ~input_type:`Number ();
              input_marker;
            ]
          | Some values ->
            if values <> [] then
              let a = (a :> Html5_types.select_attrib Eliom_content.Html5.F.attrib list) in
              let a = if hidden then a_hidden `Hidden :: a else a in
              let nothing_selected = List.for_all (fun (_,_,selected) -> not selected) values in
              let options =
                List.map
                  (function i, label, selected ->
                    Option ([], i, Some label, (nothing_selected && option_map ~f:default_constant_get value = Some i) || selected))
                  values
              in
               [
                select ~a ~required:(pcdata required_label)
                  ~name:param_names (List.hd options) (List.tl options);
                input_marker;
              ]
            else
              let a' = (a :> Html5_types.select_attrib Eliom_content.Html5.F.attrib list) in
              let open Eliom_content.Html5.F.Raw in
               [
                select ~a:(a_required `Required :: a')
                  [option ~a:[a_value ""] (pcdata required_label)]
              ]
      end
      | `Display ->
         [
          pcdata (option_get ~default:"" (option_map ~f:int_to_string value'))
        ]

module Form_int64 =
  Make_atomic
    (struct
      type a = int64
      type param_name = [`One of int64] Eliom_parameter.param_name
      let params_type = Eliom_parameter.int64
      type template_data = (int64 * pcdata * bool) list option
      type 'res template_data_fun = ?from_list:(int64 * pcdata * bool) list -> unit -> 'res
      let template_data ~value:_ ?from_list () = from_list
      let template_data_lwt ~value:_ ?from_list () = Lwt.return from_list
      let apply_template_data_fun (f : _ template_data_fun) = f ()
      let default_widget =
        let open Eliom_content.Html5.F in
        int_widget int64_input int64_select Int64.to_string
     end)

module Form_int32 =
  Make_atomic
    (struct
      type a = int32
      type param_name = [`One of int32] Eliom_parameter.param_name
      let params_type = Eliom_parameter.int32
      type template_data = (int32 * pcdata * bool) list option
      type 'res template_data_fun = ?from_list:(int32 * pcdata * bool) list -> unit -> 'res
      let template_data ~value:_ ?from_list () = from_list
      let template_data_lwt ~value:_ ?from_list () = Lwt.return from_list
      let apply_template_data_fun (f : _ template_data_fun) = f ()
      let default_widget =
        let open Eliom_content.Html5.F in
        int_widget int32_input int32_select Int32.to_string
     end)

module Form_unit =
  Make_atomic
    (struct
      type a = unit
      type param_name = unit
      include Template_data_unit (struct type t = a end)
      let params_type _ = Eliom_parameter.unit
      let default_widget =
        fun ~param_names:_ ?value:_ ?a:_ ~template_data:() () ->
          []
     end)

module Form_bool =
  Make_atomic
    (struct
      type a = bool
      type param_name = [`One of bool] Eliom_parameter.param_name
      let params_type = Eliom_parameter.bool
      type template_data = unit
      type 'res template_data_fun = 'res
      let template_data ~value:_ = ()
      let template_data_lwt ~value:_ = Lwt.return ()
      let apply_template_data_fun x = x
      let default_widget : (_,_,_) widget =
        let open Eliom_content.Html5.F in
        fun ~param_names ?value ?(a=[]) ~template_data:() () ->
          let a = (a :> Html5_types.input_attrib Eliom_content.Html5.F.attrib list) in
          let hidden, value = hidden_value value in
          match param_names with
          | `Param_names (_, param_names) ->
            let a =
              if hidden then a_hidden `Hidden :: a else a
            in [
              bool_checkbox ~a ~name:param_names ?checked:value ();
              input_marker;
            ]
          | `Display ->
            if not hidden then
              option_get_map ~default:[]
                ~f:(fun x -> list_singleton (pcdata (if x then "yes" else "no")))
                value
            else []
      let default_template = atomic_template default_template default_widget
     end)

(******************************************************************************)
type string_or_empty = string
module Form_string_or_empty =
  Make_atomic
    (struct
      include Form_string.Atomic_options
      let default_widget = form_string_default_widget true
     end)
type text_or_empty = string
module Form_text_or_empty =
  Make_atomic
    (struct
      include Form_text.Atomic_options
      let default_widget = form_text_default_widget true
     end)
}}
