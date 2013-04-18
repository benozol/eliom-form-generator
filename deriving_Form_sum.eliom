{shared{

open Deriving_Form_utils
open Deriving_Form_base

type selector_param_name = [`One of string] Eliom_parameter.param_name

type ('a, 'param_names, 'deep_config) variant =
  (module Variant with
    type enclosing_a = 'a and
    type enclosing_param_names = 'param_names and
    type enclosing_deep_config = 'deep_config)

module type Sum_options = sig
  include Base_options
  val variants : (a, param_names, deep_config) variant list
  val project_selector_param_name : param_names -> selector_param_name
end

type variant_selection = [`Drop_down(* | `Radio*)]
let default_variant_selection : variant_selection = `Drop_down

type 'template_data sum_template_data =
  variant_selection * 'template_data
type 'template_data_fun sum_template_data_fun =
  ?variant_selection:variant_selection -> unit -> 'template_data_fun

module Make
  : functor (Options : Sum_options) ->
      Form with
        type a = Options.a and
        type repr = Options.repr and
        type param_names = Options.param_names and
        type deep_config = Options.deep_config and
        type template_data = Options.template_data sum_template_data and
        type 'res template_data_fun =
          ('res Options.template_data_fun) sum_template_data_fun and
        type config =
          ( Options.a, Options.param_names,
            variant_selection * Options.template_data,
            Options.deep_config
          ) config' and
        type ('arg, 'res) opt_component_configs_fun =
          ('arg, 'res) Options.opt_component_configs_fun
= functor (Options : Sum_options) -> struct

  let template_arguments_to_options :
      (_, _, Options.template_data sum_template_data) Template.arguments ->
      (_, _, Options.template_data) Template.arguments =
    fun { Template.is_outmost; submit;
          config = { Pre_local_config.label; annotation; default; a; };
          template_data = (variant_selection, template_data);
          param_names; component_renderings
        } ->
      let a =
        let variant_selection_class =
          match variant_selection with
          | `Drop_down -> form_sum_dropdown_class
        in
        let classes =[form_class; form_sum_class; variant_selection_class] in
        Some (Eliom_content.Html5.F.a_class classes :: option_get ~default:[] a)
      in
      { Template.is_outmost; submit;
        config = Pre_local_config.({ label; annotation; default; a });
        param_names; component_renderings; template_data }

  type template_data' = Options.template_data sum_template_data
  type 'res template_data_fun' =
    ('res Options.template_data_fun) sum_template_data_fun
  module Options' = struct
    include (Options : module type of Options with
                         type template_data := Options.template_data and
                         type 'res template_data_fun := 'res Options.template_data_fun and
                         type a = Options.a and
                         type repr = Options.repr and
                         type param_names = Options.param_names and
                         type deep_config = Options.deep_config and
                         type ('arg, 'res) opt_component_configs_fun =
                           ('arg, 'res) Options.opt_component_configs_fun )
    type template_data = Options.template_data sum_template_data
    type 'res template_data_fun =
      ('res Options.template_data_fun) sum_template_data_fun
    let pre_template_data ?default : (template_data -> 'res) -> 'res template_data_fun =
      fun (k : template_data  -> _) ?(variant_selection=default_variant_selection) () ->
        Options.pre_template_data ?default (fun data -> k (variant_selection, data))
    let apply_template_data_fun ?default (f : _ template_data_fun) =
        Options.apply_template_data_fun (f ())
    let default_template =
      fun args ->
        Options.default_template (template_arguments_to_options args)
  end

  include Make_base (Options')

  let config =
    Local_config.fun_
      (fun local () ->
        Options.opt_component_configs_fun
          (fun deep () ->
            { local ; deep }))

  let variant_renderings submit (param_names : param_names or_display)
      { local ; deep } variant_selection =
    let first =
      match variant_selection with
        | `Drop_down ->
          let required_label = "Select an option" in
          let open Eliom_content.Html5.F in
          let content = [
            match param_names with
              | `Param_names (prefix, param_names) ->
                let name = Options.project_selector_param_name param_names in
                  let options =
                    List.map2
                      (fun variant_name
                        (module Variant : Variant with
                          type enclosing_a = a and
                          type enclosing_param_names = param_names and
                          type enclosing_deep_config = deep_config)
                      ->
                        let selected =
                          option_get_map ~default:false ~f:Variant.is_constructor
                            Local_config.(local.pre.default)
                        in
                        let label = pcdata (default_label_of_component_name variant_name) in
                        Eliom_content.Html5.D.Option ([], variant_name, Some label, selected))
                      Options.component_names Options.variants
                  in
                  let select =
                    Eliom_content.Html5.D.string_select
                      ~a:[a_class [form_sum_dropdown_variant_selector_class]]
                      ~name ~required:(pcdata required_label)
                      (List.hd options) (List.tl options)
                  in
                  ignore {unit{
                    Lwt_js_events.async
                      (fun () ->
                        let select_node = Eliom_content.Html5.To_dom.of_select %select in
                        Deriving_Form_base.connect_select_variant select_node;
                        Lwt.return ())
                  }};
                  div ~a:[a_class ["contains_select"]] [ select ; input_marker ]
              | `Display ->
                  let variant_name =
                    match Local_config.(local.pre.default) with
                      | None -> ""
                      | Some default ->
                        fst
                          (List.find
                             (fun (variant_name,
                                   (module Variant : Variant with
                                      type enclosing_a = a and
                                      type enclosing_param_names = param_names and
                                      type enclosing_deep_config = deep_config))
                              -> Variant.is_constructor default)
                             (List.combine Options.component_names Options.variants))
                  in
                  pcdata variant_name
          ] in
          [ Component_rendering.mk ~content () ]
    in
    first @
    list_filter_some
      (List.map2
         (fun variant_name
           (module Variant : Variant with
              type enclosing_a = a and
              type enclosing_param_names = param_names and
              type enclosing_deep_config = deep_config)
         ->
           let config =
             let config_from_deep =
               option_get
                 ~default:{ local = Local_config.mk ();
                            deep = Variant.default_deep_config }
                 (Variant.project_config deep)
             in
             let local =
               let default_local =
                 let default = option_bind Variant.project_default Local_config.(local.pre.default) in
                 let template_data =
                   Some (Variant.apply_template_data_fun (Variant.pre_template_data identity))
                 in
                 Local_config.mk' ~label:None ~annotation:None ~default
                   ~a:None ~template:None ~template_data ()
               in
               Local_config.option_or_by_field [ config_from_deep.local ; default_local ]
             in
             { config_from_deep with local }
           in
           let is_constructor =
             option_get_map ~default:false ~f:Variant.is_constructor
               Local_config.(local.pre.default)
           in
           if param_names = `Display && not is_constructor then
             None
           else
             let selector =
               match variant_selection with
                 | `Drop_down -> None
             in
             let content =
               let param_names =
                 match param_names with
                   | `Display -> `Display
                   | `Param_names (prefix, param_names) ->
                     `Param_names
                       (Variant.prefix prefix,
                        Variant.project_param_names param_names)
               in
               Variant.pre_render false submit
                 param_names config
             in
             let a = [
               Eliom_content.Html5.F.a_class [ form_sum_variant_class ] ;
               Eliom_content.Html5.Custom_data.attrib form_sum_variant_attribute variant_name ;
             ] in
             Some (Component_rendering.mk ~a ?selector
                     ?label:Local_config.(config.local.pre.label)
                     ?annotation:Local_config.(config.local.pre.annotation)
                     ~content ()))
         Options.component_names
         Options.variants)


  let pre_render is_outmost submit param_names config =
    Local_config.bind config.local
      (fun ?label ?annotation ?default ?(a=[]) ?template ?template_data:template_data' () ->
        let variant_selection, _ as template_data =
          let default () = apply_template_data_fun (pre_template_data identity) in
          option_get' ~default template_data'
        in
        let template =
          option_get
            ~default:(fun args ->
              Options.default_template
                (template_arguments_to_options args))
            template
        in
        let component_renderings = variant_renderings submit param_names config variant_selection in
        let a =
          match default with
          | Some default ->
            let variant_name, _ =
              List.find
                (fun (_, variant) ->
                  let module Variant = (val (variant : (Options.a, Options.param_names, Options.deep_config) variant)) in
                  Variant.is_constructor default)
                (List.combine Options.component_names Options.variants)
            in
            Eliom_content.Html5.Custom_data.attrib
              form_sum_variant_attribute
              variant_name
              :: a
          | None -> a
        in
        set_required_for_outmost ~is_outmost
          (template
             (Template.arguments
                ~is_outmost ?submit
                ~config:(Pre_local_config.mk ?label ?annotation ?default ~a ())
                ~param_names ~template_data ~component_renderings ())))


  let content ?submit =
    Local_config.fun_
      (fun local () ->
        Options.opt_component_configs_fun
          (fun deep () ->
            fun param_names ->
              pre_render true submit
                (`Param_names ("", param_names))
                { local ; deep }))

  let display ~value =
    Local_config.fun_
      (fun local () ->
        let local = Local_config.({
          local with
            pre = { local.pre with default = Some value }
        }) in
        Options.opt_component_configs_fun
          (fun deep () ->
            pre_render true None `Display { local ; deep }))

end
}}
