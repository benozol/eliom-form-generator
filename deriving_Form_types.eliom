{shared{
  open Deriving_Form_utils
  open Deriving_Form_base

module type Atomic_options = sig
  type a
  type param_names
  include Template_data with type a := a
  val default_template : (a, param_names, template_data) Template.t
  val params_type : string -> (a, [`WithoutSuffix], param_names) Eliom_parameter.params_type
end

module Make_atomic (Atomic_options : Atomic_options) = struct
  module Atomic_options = Atomic_options
  module Options = struct
    include Atomic_options
    let params_type' prefix = prefix, params_type prefix
    type deep_config = unit
    type repr = a
    let to_repr x = x
    let of_repr x = x
    type ('arg, 'res) opt_component_configs_fun = 'arg -> 'res
    let default_template = default_template
    let fields = []
    let component_names = []
    let default_deep_config = ()
    let opt_component_configs_fun k = k ()
  end
  include Deriving_Form_record.Make (Options)
end

let form_string_default_template can_be_empty =
  let open Eliom_content.Html5.F in
  fun arguments ->
    Template.template
      (fun ~is_outmost:_ ?submit:_ ~config ~template_data:opt_pattern
        ~param_names ~component_renderings () ->
          Pre_local_config.bind config
            (fun ?label:_ ?annotation:_ ?default ?(a=[]) () ->
              assert (component_renderings = []);
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
              in match param_names with
              | `Param_names (_, param_names) -> [
                string_input ~a ~name:param_names ?value:default ~input_type:`Text ();
                input_marker;
              ]
              | `Display -> [ pcdata (option_get ~default:"" default) ]))
      arguments

module Form_string =
  Make_atomic
    (struct
      type a = string
      type param_names = [`One of string] Eliom_parameter.param_name
      type template_data = string option
      type 'res template_data_fun = ?required_pattern:string -> unit -> 'res
      let pre_template_data ?default:_ k ?required_pattern () = k required_pattern
      let apply_template_data_fun (f : _ template_data_fun) = f ()
      let params_type = Eliom_parameter.string
      let default_template = form_string_default_template false
     end)

module Form_int =
  Make_atomic
    (struct
      type a = int
      type param_names = [`One of int] Eliom_parameter.param_name
      let params_type = Eliom_parameter.int
      type template_data = unit
      type 'res template_data_fun = 'res
      let pre_template_data ?default:_ k = k ()
      let apply_template_data_fun (f : _ template_data_fun) = f
      let default_template =
        let open Eliom_content.Html5.F in
        Template.template
          (fun ~is_outmost:_ ?submit:_ ~config ~template_data:_
            ~param_names ~component_renderings () ->
              assert (component_renderings = []);
              Pre_local_config.bind config
                (fun ?label:_ ?annotation:_ ?default ?(a=[]) () ->
                  let a = (a :> Html5_types.input_attrib Eliom_content.Html5.F.attrib list) in
                  match param_names with
                  | `Param_names (_, param_names) -> [
                    int_input ~a:(a_required `Required :: a)
                      ~name:param_names ?value:default ~input_type:`Number ();
                    input_marker;
                  ]
                  | `Display ->
                    [ pcdata (option_get ~default:"" (option_map ~f:string_of_int default))]))
     end)

module Form_int64 =
  Make_atomic
    (struct
      type a = int64
      type param_names = [`One of int64] Eliom_parameter.param_name
      let params_type = Eliom_parameter.int64
      type template_data = (int64 * pcdata * bool) list option
      type 'res template_data_fun = ?from_list:(int64 * pcdata * bool) list -> unit -> 'res
      let pre_template_data ?default:_ k ?from_list () = k from_list
      let apply_template_data_fun (f : _ template_data_fun) = f ()
      let default_template =
        let open Eliom_content.Html5.F in
        Template.template
          (fun ~is_outmost:_ ?submit:_ ~config ~template_data:values_opt
            ~param_names ~component_renderings () ->
              assert (component_renderings = []);
            Pre_local_config.bind config
              (fun ?label:_ ?annotation:_ ?default ?(a=[]) () ->
                let required_label = "please select" in
                match param_names with
                | `Param_names (_, param_names) -> begin
                  match values_opt with
                  | None ->
                    let a = (a :> Html5_types.input_attrib Eliom_content.Html5.F.attrib list) in [
                      int64_input ~a:(a_required `Required :: a)
                        ~name:param_names ?value:default ~input_type:`Number ();
                      input_marker;
                    ]
                  | Some values ->
                      if values <> [] then
                        let a = (a :> Html5_types.select_attrib Eliom_content.Html5.F.attrib list) in
                        let options =
                          List.map
                            (function i, label, selected ->
                              Option ([], i, Some label, selected))
                            values
                        in [
                          int64_select ~a ~required:(pcdata required_label)
                            ~name:param_names (List.hd options) (List.tl options);
                          input_marker;
                        ]
                      else
                        let a' = (a :> Html5_types.select_attrib Eliom_content.Html5.F.attrib list) in
                        let open Eliom_content.Html5.F.Raw in
                        [ select ~a:(a_required `Required :: a')
                            [option ~a:[a_value ""] (pcdata required_label)] ]
                end
                | `Display -> [pcdata (option_get ~default:"" (option_map ~f:Int64.to_string default))]))
     end)

module Form_unit =
  Make_atomic
    (struct
      type a = unit
      type param_names = unit
      include Template_data_unit (struct type t = a end)
      let params_type _ = Eliom_parameter.unit
      let default_template config =
        assert (config.Template.component_renderings = []);
        []
     end)

(******************************************************************************)
type string_or_empty = string
module Form_string_or_empty =
  Make_atomic
    (struct
      include Form_string.Atomic_options
      let default_template = form_string_default_template true
     end)
}}
