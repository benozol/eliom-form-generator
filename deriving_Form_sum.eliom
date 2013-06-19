{shared{

open Deriving_Form_utils
open Deriving_Form_base

type selector_param_name = [`One of string] Eliom_parameter.param_name

type ('a, 'param_names, 'deep_config) variant =
  (module Variant with
    type enclosing_a = 'a and
    type enclosing_raw_param_names = 'param_names and
    type enclosing_deep_config = 'deep_config)

module type Sum_options = sig
  include Base_options
  val variants : (a, raw_param_names, deep_config) variant list
  val project_selector_param_name : raw_param_names -> selector_param_name
end

module Make
  : functor (Options : Sum_options) ->
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
= functor (Options : Sum_options) -> struct

  include Make_base (Options)

  let variant_selector param_names value =
    let required_label = "Select an option" in
    let open Eliom_content.Html5.F in
    let content =
      match param_names with
        | `Param_names (_prefix, param_names) ->
          let name = Options.project_selector_param_name param_names in
          let options =
            List.map2
              (fun variant_name
                (module Variant : Variant with
                   type enclosing_a = a and
                   type enclosing_raw_param_names = raw_param_names and
                   type enclosing_deep_config = deep_config) ->
                let selected =
                  option_get_map ~default:false
                    ~f:(fun dc -> Variant.is_constructor (default_constant_get dc))
                    value
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
          [ div ~a:[a_class ["contains_select"]] [ select ; input_marker ] ]
        | `Display ->
          let variant_name =
            match option_map ~f:default_constant_get value with
            | None -> "???"
            | Some value ->
              fst
                (List.find
                   (fun (_variant_name,
                         (module Variant : Variant with
                            type enclosing_a = a and
                            type enclosing_raw_param_names = raw_param_names and
                            type enclosing_deep_config = deep_config))
                   -> Variant.is_constructor value)
                   (List.combine Options.component_names Options.variants))
          in
          [ pcdata variant_name ]
      in
      Component_rendering.({ content; surrounding = surrounding_zero })

  let variant_renderings submit (param_names : raw_param_names or_display)
      deep_config deep_config_override opt_value =
    Lwt.map list_filter_some
      (Lwt_list.map_p
         (fun (variant_name,
               (module Variant : Variant with
                  type enclosing_a = a and
                  type enclosing_raw_param_names = raw_param_names and
                  type enclosing_deep_config = deep_config)) ->
           let is_constructor =
             option_get_map ~default:false
               ~f:(fun dc -> Variant.is_constructor (default_constant_get dc))
               opt_value
           in
           if param_names = `Display && not is_constructor then
             Lwt.return None
           else
             let config, config_override =
               project_config_override_config variant_name
                 Variant.project_value opt_value
                 Variant.project_config { local = Local_config.zero ; deep = Variant.default_deep_config }
                 deep_config deep_config_override
             in
             lwt content =
               let param_names =
                 match param_names with
                   | `Display -> `Display
                   | `Param_names (prefix, param_names) ->
                     `Param_names
                       (Variant.prefix prefix,
                        Variant.project_param_names param_names)
               in
               Variant.pre_render false submit
                 param_names ~config ~config_override
             in
             let a = [
               Eliom_content.Html5.F.a_class [ form_sum_variant_class ] ;
               Eliom_content.Html5.Custom_data.attrib form_sum_variant_attribute variant_name ;
             ] @@ option_get ~default:[] Local_config.(config.local.pre.a) in
             let config = { config with local = Local_config.update ~a config.local } in
             Lwt.return
               (Some { Component_rendering.content;
                       surrounding = Pre_local_config.to_surrounding Local_config.(config.local.pre) }))
         (List.combine Options.component_names Options.variants))

  let default_template args =
    let drop_labels cr =
      let open Component_rendering in
      { cr with surrounding = { cr.surrounding with label = None } }
    in
    let args = Template.({
      args with component_renderings =
        List.map drop_labels args.component_renderings
    }) in
    default_template args

  let pre_render is_outmost submit param_names ~config ~config_override =
    let open Local_config in
    let local = option_or_by_field config_override.local config.local in
    let template = option_get ~default:default_template local.template in
    lwt template_data =
      let default () =
        let value = option_map ~f:default_constant_get local.pre.value in
        apply_template_data_fun (pre_template_data ~value Lwt.return)
      in
      option_get_lwt ~default local.template_data
    in
    lwt component_renderings =
      Lwt.map
        (cons (variant_selector param_names local.pre.value))
        (variant_renderings submit param_names config.deep config_override.deep local.pre.value)
    in
    let pre =
      let here_a =
        let hidden, value' = hidden_value local.pre.value in
        let a_form_sum_class = Eliom_content.Html5.F.a_class [ form_sum_class ] in
        let a_form_sum_variant =
          match value' with
          | Some default ->
            let variant_name, _ =
              List.find
                (fun (_, variant) ->
                  let module Variant =
                        (val (variant : (Options.a, Options.raw_param_names,
                                         Options.deep_config) variant))
                  in Variant.is_constructor default)
                (List.combine Options.component_names Options.variants)
            in
            [ Eliom_content.Html5.Custom_data.attrib form_sum_variant_attribute variant_name ]
          | None -> []
        in
        let maybe_style_hidden =
          if hidden then
            [ Eliom_content.Html5.F.a_style "display: none" ]
          else []
        in
        a_form_sum_class :: a_form_sum_variant @@ maybe_style_hidden
      in
      { local.pre with a = Some (here_a @@ option_get ~default:[] local.pre.a) }
    in
    Lwt.map (set_required_for_outmost ~is_outmost)
      (template
         (Template.arguments ~is_outmost ?submit ~config:pre
            ~param_names ~template_data ~component_renderings ()))

  let content = pre_content pre_render
  let display = pre_display pre_render

end
}}
