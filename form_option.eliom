{shared{
  open Deriving_Form_base
}}

{shared{
  module Make (Form : Form) = struct
    module Options = struct
      type a = Form.a option
      module Component_None = Deriving_Form_types.Form_unit
      module Component_Some = Form
      type raw_param_names =
        [`One of string] Eliom_parameter.param_name *
          (Component_None.raw_param_names * Component_Some.raw_param_names)
      type deep_config = Component_None.config option * Component_Some.config option
      let default_deep_config = None, None
      include Template_data_unit (struct type t = a end)
      type raw_repr = string * (Component_None.raw_repr option * Component_Some.raw_repr option)
      let component_names = [ "None" ; "Some" ]
      let of_raw_repr = function
        | ("None", (Some _, _)) -> None
        | ("Some", (_, Some component)) ->
            Some (Component_Some.of_raw_repr component)
        | _ -> failwith "Form_option: of_raw_repr"
      let to_raw_repr =
        function
        | None -> ("None", ((Some (Component_None.to_raw_repr ())), None))
        | Some component ->
          ("Some", (None, (Some (Component_Some.to_raw_repr component))))
      let prefix_None prefix = prefix
      let prefix_Some prefix = prefix
      let params' prefix =
        prefix,
        Eliom_parameter.prod
          (Eliom_parameter.string (prefix ^ "||constructor"))
          (Eliom_parameter.prod
             (Eliom_parameter.opt
                (snd (Component_None.params' (prefix_None prefix))))
             (Eliom_parameter.opt
                (snd (Component_Some.params' (prefix_Some prefix)))))
      type ('arg, 'res) opt_component_configs_fun =
        ?none: Component_None.config ->
        ?some: Component_Some.config -> 'arg -> 'res
      let opt_component_configs_fun k ?none ?some arg = k (none, some) arg
      let default_template = default_template
      let variants : ((a, raw_param_names, deep_config) Deriving_Form_sum.variant) list =
        let project_None (none, _) = none
        and project_Some (_, some) = some
        in
        let module Component_None = struct
          type enclosing_a = a
          type enclosing_raw_param_names = raw_param_names
          type enclosing_deep_config = deep_config
          let project_value =
            function | None -> Some () | Some _ -> None
          let project_param_names param_names =
            project_None (snd param_names)
          let project_config = project_None
          let is_constructor =
            function | None -> true | Some _ -> false
          let prefix = prefix_None
          include Component_None
        end in
        let module Component_Some = struct
          type enclosing_a = a
          type enclosing_raw_param_names = raw_param_names
          type enclosing_deep_config = deep_config
          let project_value =
            function
            | None -> None
            | Some component -> Some component
          let project_param_names param_names =
            project_Some (snd param_names)
          let project_config = project_Some
          let is_constructor =
            function | None -> false | Some _ -> true
          let prefix = prefix_Some
          include Component_Some
        end in
        [ (module Component_None); (module Component_Some) ]
      let project_selector_param_name = fst
    end
    include Deriving_Form_sum.Make (Options)
  end
}}
