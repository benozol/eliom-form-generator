(*
  = TODO =
   - disconnect param_names and type of params_type (for Form_unit)
*)

let failwith fmt =
  Printf.ksprintf
    (fun str ->
      Ocsigen_messages.console (fun () -> str);
      failwith str)
    fmt

type div_content = Html5_types.div_content Eliom_content.Html5.elt list
type form_content = Html5_types.form_content Eliom_content_core.Html5.elt list
type button_content = Html5_types.button_content Eliom_content_core.Html5.elt list
let pcdata = Eliom_content.Html5.F.pcdata

let option_get ~default = function
  | Some x -> x
  | None -> default
let option_map ~f = function
  | Some x -> Some (f x)
  | None -> None
let option_to_list = function
  | Some x -> [x]
  | None -> []
let rec option_or = function
  | [] -> None
  | None :: rest -> option_or rest
  | some :: _ -> some

module String_map = struct
  include Map.Make (String)
  let get ~default key map =
    try find key map
    with Not_found -> default key
  let get_option key map =
    try Some (find key map)
    with Not_found -> None
  let rec find' key = function
    | [] -> raise Not_found
    | map :: maps ->
      try find key map
      with Not_found -> find' key maps
  let from_list li =
    List.fold_right (fun (key, value) -> add key value) li empty
  let from_options_list li =
    List.fold_right
      (fun (key, value) sofar ->
        match value with
        | Some value -> add key value sofar
        | None -> sofar)
      li empty
end

module Field_rendering = struct
  type t = {
    label : form_content option;
    content : form_content;
    annotation : form_content option;
  }
end

type ('a, 'param_names) template_fun =
  is_outmost:bool -> ?submit:button_content -> ?label:form_content ->
  ?annotation:form_content -> ?default:'a -> param_names:'param_names -> Field_rendering.t list -> form_content

type ('a, 'param_names) template =
  | Template of ('a, 'param_names) template_fun

let template_concat_fun =
  let open Eliom_content.Html5.F in
  fun ~is_outmost ?submit ?label ?annotation ?default ~param_names field_renderings ->
    List.map
      (fun { Field_rendering.label ; content ; annotation } ->
        div [
          div (option_get ~default:[] label) ;
          div content ;
          div (option_get ~default:[] annotation) ;
        ])
      field_renderings

let maybe_get_option_map really opt f =
  if really then
    option_get ~default:[] (option_map f opt)
  else []

let template_table_fun =
  let open Eliom_content.Html5.F in
  fun ~is_outmost ?submit ?label ?annotation ?default ~param_names field_renderings ->
    let captions =
      maybe_get_option_map is_outmost label
        (fun label ->
          [tr ~a:[a_class ["field"]]
              [td ~a:[a_class ["form_label"]; a_colspan 3] label]])
    in
    let annotations =
      maybe_get_option_map is_outmost annotation
        (fun annotation ->
          [tr ~a:[a_class ["field"]]
              [td ~a:[a_class ["form_annotation"]; a_colspan 3] annotation]])
    in
    let submits =
      maybe_get_option_map is_outmost submit
        (fun submit ->
          [tr ~a:[a_class ["field"]]
              [td ~a:[a_class ["form_submit"]; a_colspan 3]
                  [button ~button_type:`Submit submit]]])
    in
    let fields =
      List.map
        (fun { Field_rendering.label ; content ; annotation } ->
          let label =
            [ td ~a:[a_class ["label"]]
                (option_get ~default:[] label) ]
          in
          let content = td ~a:[a_class ["content"]] content in
          let annotation =
            [ td ~a:[a_class ["annotation"]]
                (option_get ~default:[] annotation) ]
          in
          tr ~a:[a_class ["field"]]
            (label @ content :: annotation))
        field_renderings
    in
    let contents = captions @ fields @ annotations @ submits in
    let outmost_class = if is_outmost then [a_class ["outmost"]] else [] in
    [ table ~a:(a_class ["form"] :: outmost_class)
        (List.hd contents) (List.tl contents) ]

let default_template =
  Template template_table_fun

module Config = struct
  type ('a, 'param_names, 'deep_config) local = {
    label : form_content option;
    annotation : form_content option;
    default : 'a option;
    template : ('a, 'param_names) template option;
  }
  let local_zero = { label = None; annotation = None; default = None; template = None }
  let option_or_by_field c1 c2 = {
    label = option_or [c1.label; c2.label];
    annotation = option_or [c1.annotation; c2.annotation];
    default = option_or [c1.default; c2.default];
    template = option_or [c1.template; c2.template];
  }
  type ('a, 'param_names, 'deep_config) t = {
    local : ('a, 'param_names, 'deep_config) local;
    deep : 'deep_config;
  }
  type ('a, 'param_names, 'deep_config, 'arg, 'res) local_fun =
    ?label:form_content ->
    ?annotation:form_content ->
    ?default:'a ->
    ?template:('a, 'param_names) template ->
    'arg -> 'res
  let local_fun k
      ?label
      ?annotation
      ?default
      ?template
      arg
      =
    k { label ; default ; annotation ; template } arg
end

module type Tuple = sig
  type a
  type tuple
  val from_tuple : tuple -> a
  val to_tuple : a -> tuple
end

module type Pre_form = sig
  type a
  type param_names
  type deep_config
  type config = (a, param_names, deep_config) Config.t
  val field_renderings : button_content option -> param_names -> config -> Field_rendering.t list
  val pre_render : bool -> button_content option -> param_names -> config -> form_content
  include Tuple with type a := a
  val params_type : string -> (tuple, [`WithoutSuffix], param_names) Eliom_parameter.params_type
  val default_deep_config : deep_config
end

module type Form = sig
  include Pre_form
  type ('arg, 'res) opt_field_configs_fun
  val content :
    ?submit:button_content ->
    (a, param_names, deep_config, unit,
     (param_names, form_content) opt_field_configs_fun) Config.local_fun
  val field :
    (a, param_names, deep_config, unit,
     (unit, config) opt_field_configs_fun) Config.local_fun
  val handler : (a -> 'res) -> (tuple -> 'res)
end

module type Field = sig
  include Pre_form
  type enclosing_tuple
  type enclosing_param_names
  type enclosing_deep_config
  val project_tuple : enclosing_tuple -> tuple
  val project_param_names : enclosing_param_names -> param_names
  val project_config : enclosing_deep_config -> config option
end

type ('tuple, 'param_names, 'deep_config) field =
  (module Field
    with type enclosing_tuple = 'tuple
    and type enclosing_param_names = 'param_names
    and type enclosing_deep_config = 'deep_config)

module type Options = sig
  type a
  type param_names
  type deep_config
  val default_deep_config : deep_config
  include Tuple with type a := a
  val params_type : string -> (tuple, [`WithoutSuffix], param_names) Eliom_parameter.params_type
  val fields : (tuple, param_names, deep_config) field list
  val field_names : string list
  type ('arg, 'res) opt_field_configs_fun
  val opt_field_configs_fun : (deep_config -> 'arg -> 'res) -> ('arg, 'res) opt_field_configs_fun
  val default_template : (a, param_names) template
end

module Make :
  functor (Options : Options) -> Form
    with type a = Options.a
    and type tuple = Options.tuple
    and type param_names = Options.param_names
    and type deep_config = Options.deep_config
    and type config = (Options.a, Options.param_names, Options.deep_config) Config.t
    and type ('arg, 'res) opt_field_configs_fun = ('arg, 'res) Options.opt_field_configs_fun
=
  functor (Options : Options) -> struct
    include Options
    type config = (a, param_names, deep_config) Config.t

    let field_renderings submit param_names { Config.local ; deep } =
      List.map2
        (fun field_name (module Field : Field
                with type enclosing_tuple = tuple
                and type enclosing_param_names = param_names
                and type enclosing_deep_config = deep_config) ->
          let default_local =
            let label = Some [pcdata field_name] in
            let default =
              let project default =
                Field.from_tuple (Field.project_tuple (to_tuple default))
              in
              option_map project local.Config.default
            in
            { Config.label ; default ; template = None; annotation = None }
          in
          let config =
            let default = { Config.local = default_local ; deep = Field.default_deep_config } in
            option_get ~default
              (option_map
                 (fun config ->
                   let local = Config.option_or_by_field config.Config.local default_local in
                   { config with Config.local = local })
                 (Field.project_config deep))
          in
          let param_names = Field.project_param_names param_names in
          let content = Field.pre_render false submit param_names config in
          { Field_rendering.label = config.Config.local.Config.label;
            annotation = config.Config.local.Config.annotation;
            content })
        Options.field_names
        Options.fields

    let pre_render is_outmost submit param_names config =
      let { Config.label ; annotation ; default ; template } = config.Config.local in
      let Template template_fun =
        option_get ~default:Options.default_template template
      in
      template_fun ~is_outmost ?submit ?label ?annotation ?default ~param_names
        (field_renderings submit param_names config)

    let func k ?submit =
      Config.local_fun
        (fun local () ->
          Options.opt_field_configs_fun
            (fun deep deep_arg ->
              k submit deep_arg { Config.local ; deep }))
    let content = func (pre_render true)
    let field = func (fun submit () config -> assert (submit = None); config) ?submit:None
    let handler f =
      fun tuple ->
        f (from_tuple tuple)
  end

module type Atomic_options = sig
  type a
  type param_names
  val template_fun : (a, param_names) template_fun
  val params_type : string -> (a, [`WithoutSuffix], param_names) Eliom_parameter.params_type
end

module Make_atomic_options (Atomic_options : Atomic_options) = struct
  include Atomic_options
  type deep_config = unit
  type tuple = a
  let to_tuple x = x
  let from_tuple x = x
  type ('arg, 'res) opt_field_configs_fun = 'arg -> 'res
  let default_template = Template template_fun
  let fields = []
  let field_names = []
  let default_deep_config = ()
  let opt_field_configs_fun f x = f () x
end

module Form_string = struct
  module Options =
    Make_atomic_options
      (struct
        type a = string
        type param_names = [`One of string] Eliom_parameter.param_name
        let params_type = Eliom_parameter.string
        let template_fun ~is_outmost ?submit ?label ?annotation ?default ~param_names _ =
          [ Eliom_content.Html5.F.string_input ~name:param_names
              ?value:default ~input_type:`Text () ]
       end)
  include Make (Options)
end

module Form_int = struct
  module Options =
    Make_atomic_options
      (struct
        type a = int
        type param_names = [`One of int] Eliom_parameter.param_name
        let params_type = Eliom_parameter.int
        let template_fun ~is_outmost ?submit ?label ?annotation ?default ~param_names _ =
          [ Eliom_content.Html5.F.int_input ~name:param_names
              ?value:default ~input_type:`Number () ]
       end)
  include Make (Options)
end

module Form_unit = struct
  module Options =
    Make_atomic_options
      (struct
        type a = unit
        type param_names = unit
        let params_type _ = Eliom_parameter.unit
        let template_fun ~is_outmost ?submit ?label ?annotation ?default ~param_names _ =
          []
       end)
  include Make (Options)
end

(* module Form_list = struct *)
(*   module Make : *)
(*     functor (Options:Options) -> Form *)
(*   = *)
(*     functor (Options:Options) -> struct *)
(*       module Options = struct *)
(*         type a = Options.a list *)
(*         type tuple = *)
(*       end *)
(*     end *)
(* end *)

module Form_option = struct

  let field deep =
    Config.({ local = Config.local_zero ; deep = Some deep })

  module Make :
    functor (Options:Options) -> Form
      with type a = Options.a option
      and type tuple = bool * Options.tuple option
      and type param_names = [`One of bool] Eliom_parameter.param_name * Options.param_names
      and type deep_config = (Options.a, Options.param_names, Options.deep_config) Config.t option
      and type ('arg, 'res) opt_field_configs_fun =
            (Options.a, Options.param_names, Options.deep_config) Config.t option -> 'arg -> 'res
  =
    functor (Options:Options) -> struct
      module Options = struct
        type a = Options.a option
        type tuple = bool * Options.tuple option
        type param_names =
          [`One of bool] Eliom_parameter.param_name *
            Options.param_names
        let params_type prefix =
          Eliom_parameter.prod
            (Eliom_parameter.bool (prefix^"_is_some"))
            (Eliom_parameter.neopt (Options.params_type (prefix^"_some")))
        type deep_config = (Options.a, Options.param_names, Options.deep_config) Config.t option
        let default_deep_config = None
        let to_tuple = function
          | None -> false, None
          | Some x -> true, Some (Options.to_tuple x)
        let from_tuple = function
          | (false, _) -> None
          | (true, Some x) -> Some (Options.from_tuple x)
          | _ -> failwith "Form_option_functor.from_tuple"
        type ('arg, 'res) opt_field_configs_fun =
            deep_config -> 'arg -> 'res
        let opt_field_configs_fun f = f
        let field_names = [ "some" ]
        let fields =
          let module Field = struct
            type enclosing_tuple = tuple
            type enclosing_param_names = param_names
            type enclosing_deep_config = deep_config
            let project_tuple = function
              | (false, Some tuple) -> tuple
              | _ -> failwith "Form_option_functor.Field.project_tuple"
            let project_param_names (_, param_names) =
              param_names
            let project_config deep_config = deep_config
            include Make (Options)
          end in
          [ ((module Field) : (tuple, param_names, deep_config) field) ]
        let default_template =
          let open Eliom_content.Html5.F in
          Template
            (fun ~is_outmost ?submit ?label ?annotation ?default ~param_names field_renderings ->
              let (name, param_names) = param_names in
              let captions =
                maybe_get_option_map is_outmost label
                  (fun label ->
                    [div ~a:[a_class ["form_label"]] label])
              in
              let contents =
                let field = bool_checkbox ~name () in
                let fields =
                  template_table_fun ~is_outmost ~param_names ?submit
                    (List.map (fun r -> { r with Field_rendering.label = None })
                       field_renderings)
                in
                let labels =
                  maybe_get_option_map is_outmost label
                    (fun label ->
                      [ div ~a:[a_class ["label"]] label ])
                in
                let annotations =
                  maybe_get_option_map is_outmost annotation
                    (fun annotation ->
                      [ div ~a:[a_class ["annotation"]] annotation ])
                in [
                  div ~a:[a_class ["field"]] (labels @ field :: annotations);
                  div ~a:[a_class ["field"]] fields;
                ]
              in
              let annotations =
                maybe_get_option_map is_outmost annotation
                  (fun annotation ->
                    [div ~a:[a_class ["form_annotation"]] annotation])
              in
              let submits =
                maybe_get_option_map is_outmost submit
                  (fun submit ->
                    [div ~a:[a_class ["form_submit"]]
                        [button ~button_type:`Submit submit]])
              in
              [ div ~a:[a_class ["form"]] (captions @ contents @ annotations @ submits) ])
      end
      include Make (Options)
    end
end
