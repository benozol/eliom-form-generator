{shared{
open Deriving_Form_utils
open Deriving_Form_base

type ('a, 'param_names, 'deep_config) field =
  (module Field with
    type enclosing_a = 'a and
    type enclosing_raw_param_names = 'param_names and
    type enclosing_deep_config = 'deep_config)

module type Record_options = sig
  include Base_options
  val fields : (a, raw_param_names, deep_config) field list
end

module Make :
  functor (Options : Record_options) ->
    Form with
      type a = Options.a and
      type raw_repr = Options.raw_repr and
      type raw_param_names = Options.raw_param_names and
      type deep_config = Options.deep_config and
      type template_data = Options.template_data and
      type 'res template_data_fun = 'res Options.template_data_fun and
      type config =
        ( Options.a, Options.raw_param_names,
          Options.template_data, Options.deep_config
        ) config' and
      type ('arg, 'res) opt_component_configs_fun =
        ('arg, 'res) Options.opt_component_configs_fun
=
  functor (Options : Record_options) -> struct

    include Make_base (Options)

    module Pre (Monad : Monad_with_template_data) =
    struct
      let field_renderings submit param_names deep_config deep_config_override opt_value =
        Monad.List.map
          (fun (field_name,
                (module Field : Field with
                   type enclosing_a = a and
                   type enclosing_raw_param_names = raw_param_names and
                   type enclosing_deep_config = deep_config)) ->
            let module Field_pre = Field.Pre (Monad) in
            let config, config_override =
              project_config_override_config field_name
                Field.project_value opt_value
                Field.project_config { local = Local_config.zero ; deep = Field.default_deep_config }
                deep_config deep_config_override
            in
            Monad.bind
              (let param_names =
                 match param_names with
                   | `Display -> `Display
                   | `Param_names (prefix, param_names) ->
                     `Param_names
                       (Field.prefix prefix,
                        Field.project_param_names param_names)
               in
               Field_pre.pre_render false submit param_names ~config ~config_override)
            @ fun content ->
                Monad.return
                  { Component_rendering.content;
                    surrounding = Pre_local_config.to_surrounding Local_config.(config.local.pre) })
          (List.combine Options.component_names Options.fields)


      let pre_render is_outmost submit param_names ~config ~config_override =
        let open Local_config in
        let local = option_or_by_field config_override.local config.local in
        let template = option_get ~default:Options.default_template local.template in
        Monad.bind
          (let default () =
             let value = option_map ~f:default_constant_get local.pre.value in
             let module Template_data = Monad.Template_data (Options) in
             Template_data.template_data ~value
           in
           option_get' ~default @
             option_map ~f:Monad.return local.template_data)
        @ fun template_data ->
            let pre =
              let a =
                let classes = Eliom_content.Html5.F.a_class [form_class; form_record_class] in
                let maybe_hidden =
                  if option_map ~f:default_or_constant local.pre.value = Some `Constant then
                    [ Eliom_content.Html5.F.a_style "display:  none" ]
                  else
                    []
                in
                classes :: maybe_hidden @@ option_get ~default:[] local.pre.a
              in
              { local.pre with a = Some a } in
            Monad.bind
              (field_renderings submit param_names config.deep config_override.deep local.pre.value)
            @ fun component_renderings ->
              Monad.return @ set_required_for_outmost ~is_outmost @ template @
                  Template.arguments ~is_outmost ?submit ~config:pre
                    ~param_names ~template_data ~component_renderings ()
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
}}
