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

    let field_renderings submit param_names { local ; deep } =
      List.map2
        (fun field_name
          (module Field : Field with
             type enclosing_a = a and
             type enclosing_param_names = param_names and
             type enclosing_deep_config = deep_config) ->
          let config =
            let config_from_deep =
              option_get
                ~default:{ local = Local_config.mk () ; deep = Field.default_deep_config }
                (Field.project_config deep)
            in
            let local =
              let default_local =
                let label = Some [pcdata (default_label_of_component_name field_name)] in
                let default = option_bind Field.project_default Local_config.(local.pre.default) in
                let template_data = Some (Field.apply_template_data_fun (Field.pre_template_data identity)) in
                Local_config.mk' ~label ~default ~template_data
                  ~annotation:None ~a:None ~template:None ()
              in
              Local_config.option_or_by_field [ config_from_deep.local ; default_local ]
            in
            { config_from_deep with local }
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
            ?label:Local_config.(config.local.pre.label)
            ?annotation:Local_config.(config.local.pre.annotation)
            ~content ())
        Options.component_names
        Options.fields

    let config =
      Local_config.fun_
        (fun local () ->
          Options.opt_component_configs_fun
            (fun deep () ->
              { local ; deep }))

    let pre_render is_outmost submit param_names config =
      Local_config.bind config.local
        (fun ?label ?annotation ?default ?(a=[]) ?template ?template_data:template_data' () ->
          let template_data =
            let default () =
              apply_template_data_fun (pre_template_data ?default identity)
            in
            option_get' ~default template_data'
          in
          let template = option_get ~default:Options.default_template template in
          let a =
            Some (Eliom_content.Html5.F.a_class
                     [form_class; form_record_class]
                     :: a)
          in
          set_required_for_outmost ~is_outmost
            (template
               (Template.arguments
                  ~is_outmost ?submit
                  ~config:(Pre_local_config.mk ?label ?annotation ?default ?a ())
                  ~param_names ~template_data
                  ~component_renderings:(field_renderings submit param_names config)
                  ())))


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
            local with pre =
              { local.pre with default = Some value }
          }) in
          Options.opt_component_configs_fun
            (fun deep () ->
              pre_render true None
                `Display
                { local ; deep }))

  end
}}
