{shared{
open Deriving_Form_utils
open Deriving_Form_base

type ('a, 'param_names, 'deep_config) field =
  (module Field with
    type enclosing_a = 'a and
    type enclosing_param_names = 'param_names and
    type enclosing_deep_config = 'deep_config)

module type Record_options = sig
  include Base_options
  val fields : (a, param_names, deep_config) field list
end

module Make :
  functor (Options : Record_options) ->
    Form with
      type a = Options.a and
      type repr = Options.repr and
      type param_names = Options.param_names and
      type deep_config = Options.deep_config and
      type template_data = Options.template_data and
      type 'res template_data_fun = 'res Options.template_data_fun and
      type config =
        ( Options.a, Options.param_names,
          Options.template_data, Options.deep_config
        ) config' and
      type ('arg, 'res) opt_component_configs_fun =
        ('arg, 'res) Options.opt_component_configs_fun
=
  functor (Options : Record_options) -> struct

    include Make_base (Options)

    let field_renderings submit param_names value deep =
      List.map
        (fun (module Field : Field with
                type enclosing_a = a and
                type enclosing_param_names = param_names and
                type enclosing_deep_config = deep_config) ->
         let config =
           let default = { local = Local_config.zero ; deep = Field.default_deep_config } in
           let config = option_get ~default (Field.project_config deep) in
           let value = option_or [
             Local_config.(config.local.pre.value);
             option_bind ~f:default_constant_put_over_option
               (option_map ~f:(default_constant_map ~f:Field.project_value)
                  value)
           ] in
           Local_config.({
             config with local = {
               config.local with pre = {
                 config.local.pre with value
               }
             }
           })
         in
         let content =
           let param_names =
             match param_names with
             | `Display -> `Display
             | `Param_names (prefix, param_names) ->
               `Param_names
                 (Field.prefix prefix,
                  Field.project_param_names param_names)
           in
           Field.pre_render false submit param_names config
         in
         Pre_local_config.bind config.local.Local_config.pre
           (fun ?label ?annotation ?value ?a () ->
             let default_constant = option_map ~f:default_constant value in
             { Component_rendering.
               content; default_constant; a; label; annotation;
               selector=None }))
        Options.fields

    let pre_render is_outmost submit param_names { local ; deep } =
      let { Local_config.pre ; template ; template_data } = local in
      let template = option_get ~default:Options.default_template template in
      let template_data =
        let default () =
          let value = option_map ~f:default_constant_get pre.Local_config.value in
          apply_template_data_fun (pre_template_data ~value identity)
        in
        option_get' ~default template_data
      in
      let pre =
        let a =
          let classes = Eliom_content.Html5.F.a_class [form_class; form_record_class] in
          let maybe_hidden =
            if option_map ~f:default_constant pre.Local_config.value = Some `Constant then
              [ Eliom_content.Html5.F.a_style "display:  none" ]
            else
              []
          in
          classes :: maybe_hidden @ option_get ~default:[] pre.Local_config.a
        in
        { pre with Local_config.a = Some a } in
      let component_renderings = field_renderings submit param_names pre.Local_config.value deep in
      set_required_for_outmost ~is_outmost
        (template
           (Template.arguments ~is_outmost ?submit ~config:pre
              ~param_names ~template_data ~component_renderings ()))

    let content = pre_content pre_render
    let display = pre_display pre_render
  end
}}
