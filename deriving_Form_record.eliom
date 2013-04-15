{shared{

open Deriving_Form_base

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
        ( Options.a, Options.param_names, Options.deep_config, Options.template_data
        ) Config.t and
      type ('arg, 'res) opt_component_configs_fun =
        ('arg, 'res) Options.opt_component_configs_fun
=
  functor (Options : Record_options) -> struct

    include Make_base (Options)

    let field_renderings submit param_names { Config.local ; deep } =
      List.map2
        (fun field_name
          (module Field : Field with
             type enclosing_a = a and
             type enclosing_param_names = param_names and
             type enclosing_deep_config = deep_config) ->
          let config =
            let config_from_deep =
              option_get
                ~default:{ Config.local = Config.local_zero ; deep = Field.default_deep_config }
                (Field.project_config deep)
            in
            let local =
              let default_local =
                let label = Some [pcdata (default_label_of_component_name field_name)] in
                let default = option_bind Field.project_default local.Config.default in
                let template_data = Some (Field.apply_template_data_fun (Field.pre_template_data identity)) in
                { Config.label ; default ; template_data ;
                  template = None ; annotation = None ; classes = None }
              in
              Config.option_or_by_field [ config_from_deep.Config.local ; default_local ]
            in
            { config_from_deep with Config.local }
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
          Component_rendering.mk
            ?label:Config.(config.local.label)
            ?annotation:Config.(config.local.annotation)
            ~content ())
        Options.component_names
        Options.fields

    let config =
      Config.local_fun
        (fun local () ->
          Options.opt_component_configs_fun
            (fun deep () ->
              { Config.local ; deep }))

    let pre_render is_outmost submit param_names config =
      let { Config.label ; annotation ; default ; template ; classes ; template_data = template_data_opt } = config.Config.local in
      let template_data = option_get' ~default:(fun () -> apply_template_data_fun template_data) template_data_opt in
      let template = option_get ~default:Options.default_template template in
      let a =
        Some [Eliom_content.Html5.F.a_class
                (form_class :: form_record_class ::
                   option_get ~default:[] classes)]
      in
      set_required_for_outmost ~is_outmost
        (template
           (Template.arguments
              ~is_outmost ?submit ?label ?annotation
              ?default ~param_names ~template_data ?a
              (field_renderings submit param_names config)))


    let content ?submit =
      Config.local_fun
        (fun local () ->
          Options.opt_component_configs_fun
            (fun deep () ->
              fun param_names ->
                pre_render true submit
                  (`Param_names ("", param_names))
                  { Config.local ; deep }))

    let display ~value =
      Config.local_fun
        (fun local () ->
          let local = { local with Config.default = Some value } in
          Options.opt_component_configs_fun
            (fun deep () ->
              pre_render true None
                `Display
                { Config.local ; deep }))

  end
}}
