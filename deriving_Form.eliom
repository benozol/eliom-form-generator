(*
  = TODO =
   - Rename tuple to repr (namely Eliom's)
*)

{server{
let debug fmt =
  Printf.ksprintf
    (fun str ->
      Ocsigen_messages.console (fun () -> str))
    fmt

let failwith fmt =
  Printf.ksprintf
    (fun str ->
      Ocsigen_messages.console (fun () -> str);
      failwith str)
    fmt
}}

{shared{
type div_content = Html5_types.div_content Eliom_content.Html5.elt list
type form_content = Html5_types.form_content Eliom_content_core.Html5.elt list
type button_content = Html5_types.button_content Eliom_content_core.Html5.elt list
let pcdata = Eliom_content.Html5.F.pcdata

let option_get ~default = function
  | Some x -> x
  | None -> default
let option_get' ~default = function
  | Some x -> x
  | None -> default ()
let option_map ~f = function
  | Some x -> Some (f x)
  | None -> None
let option_iter f = function
  | Some x -> f x
  | None -> ()
let option_to_list = function
  | Some x -> [x]
  | None -> []
let option_bind f = function
  | Some x -> f x
  | None -> None
let some x = Some x
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
}}

module Field_rendering = struct
  type t = {
    label : form_content option;
    content : form_content;
    annotation : form_content option;
  }
end

type ('a, 'param_names, 'template_data) template =
  is_outmost:bool -> ?submit:button_content -> ?label:form_content ->
  ?annotation:form_content -> ?default:'a -> ?classes:string list ->
  ?template_data:'template_data -> param_names:'param_names -> Field_rendering.t list ->
  form_content

let template_concat : 'a 'param_names 'template_data . ('a, 'param_names, 'template_data) template =
  let open Eliom_content.Html5.F in
  fun ~is_outmost ?submit ?label ?annotation ?default ?(classes=[]) ?template_data ~param_names field_renderings ->
    [ div ~a:[a_class classes]
        (List.map
           (fun { Field_rendering.label ; content ; annotation } ->
             div [
               div (option_get ~default:[] label) ;
               div content ;
               div (option_get ~default:[] annotation) ;
             ])
           field_renderings) ]

let maybe_get_option_map really opt f =
  if really then
    option_get ~default:[] (option_map f opt)
  else []

let template_table : (_, _, _) template =
  let open Eliom_content.Html5.F in
  fun ~is_outmost ?submit ?label ?annotation ?default ?(classes=[]) ?template_data ~param_names field_renderings ->
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
              [td [];
               td ~a:[a_class ["form_submit"]; a_colspan 3]
                 [button ~button_type:`Submit submit];
               td []]])
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
    let outmost_class = if is_outmost then ["outmost"] else [] in
    [ table ~a:[a_class ("form" :: outmost_class @ classes)]
        (List.hd contents) (List.tl contents) ]

let default_template =
  template_table

let default_label_of_field_name s =
  let s =
    if Str.string_match (Str.regexp "[a-z]_+") s 0
    then
      let prefix_length = String.length (Str.matched_string s) in
      String.sub s prefix_length (String.length s - prefix_length)
    else s
  in
  let s = Str.global_replace (Str.regexp "_") " " s in
  String.capitalize s

module Config = struct
  type ('a, 'param_names, 'deep_config, 'template_data) local = {
    label : form_content option;
    annotation : form_content option;
    default : 'a option;
    template : ('a, 'param_names, 'template_data) template option;
    template_data : 'template_data option;
  }
  let local_zero = {
    label = None; annotation = None; default = None;
    template = None ; template_data = None
  }
  let rec option_or_by_field = function
    | [] -> local_zero
    | c1 :: [] -> c1
    | c1 :: c2 :: rest ->
      option_or_by_field
        ({ label = option_or [c1.label; c2.label];
           annotation = option_or [c1.annotation; c2.annotation];
           default = option_or [c1.default; c2.default];
           template = option_or [c1.template; c2.template];
           template_data = option_or [c1.template_data; c2.template_data];
         } :: rest)
  type ('a, 'param_names, 'deep_config, 'template_data) t = {
    local : ('a, 'param_names, 'deep_config, 'template_data) local;
    deep : 'deep_config;
  }
  type ('a, 'param_names, 'deep_config, 'template_data, 'arg, 'res) local_fun =
    ?label:form_content ->
    ?annotation:form_content ->
    ?default:'a ->
    ?template:('a, 'param_names, 'template_data) template ->
    ?template_data:'template_data ->
    'arg -> 'res
  let local_fun k ?label ?annotation ?default ?template ?template_data arg =
    k { label ; default ; annotation ; template ; template_data } arg
end

module type Repr = sig
  type a
  type repr
  val from_repr : repr -> a
  val to_repr : a -> repr
end

module type Pre_form = sig
  type a
  type param_names
  type deep_config
  type template_data
  val default_template_data : ?default:a -> unit -> template_data option
  type config = (a, param_names, deep_config, template_data) Config.t
  val field_renderings : button_content option -> param_names -> config -> Field_rendering.t list
  val pre_render : bool -> button_content option -> param_names -> config -> form_content
  include Repr with type a := a
  val params_type : string -> (repr, [`WithoutSuffix], param_names) Eliom_parameter.params_type
  val default_deep_config : deep_config
end

module type Form = sig
  include Pre_form
  type ('arg, 'res) opt_field_configs_fun
  val content :
    ?submit:button_content ->
    (a, param_names, deep_config, template_data, unit,
     (param_names, form_content) opt_field_configs_fun) Config.local_fun
  val field :
    (a, param_names, deep_config, template_data, unit,
     (unit, config) opt_field_configs_fun) Config.local_fun
  val get_handler : (a -> 'post -> 'res) -> (repr -> 'post -> 'res)
  val post_handler : ('get -> a -> 'res) -> ('get -> repr -> 'res)
end

module type Field = sig
  include Pre_form
  type enclosing_a
  type enclosing_param_names
  type enclosing_deep_config
  val project_default : enclosing_a -> a option
  val project_param_names : enclosing_param_names -> param_names
  val project_config : enclosing_deep_config -> config option
end

type ('a, 'param_names, 'deep_config) field =
  (module Field
    with type enclosing_a = 'a
    and type enclosing_param_names = 'param_names
    and type enclosing_deep_config = 'deep_config)

module type Options = sig
  type a
  type param_names
  type deep_config
  val default_deep_config : deep_config
  type template_data
  val default_template_data : ?default:a -> unit -> template_data option
  include Repr with type a := a
  val params_type : string -> (repr, [`WithoutSuffix], param_names) Eliom_parameter.params_type
  val fields : (a, param_names, deep_config) field list
  val field_names : string list
  type ('arg, 'res) opt_field_configs_fun
  val opt_field_configs_fun : (deep_config -> 'arg -> 'res) -> ('arg, 'res) opt_field_configs_fun
  val default_template : (a, param_names, template_data) template
end

module Make :
  functor (Options : Options) -> Form
    with type a = Options.a
    and type repr = Options.repr
    and type param_names = Options.param_names
    and type deep_config = Options.deep_config
    and type template_data = Options.template_data
    and type config = (Options.a, Options.param_names, Options.deep_config, Options.template_data) Config.t
    and type ('arg, 'res) opt_field_configs_fun = ('arg, 'res) Options.opt_field_configs_fun
=
  functor (Options : Options) -> struct

    include Options

    type config = (a, param_names, deep_config, template_data) Config.t

    let field_renderings submit param_names { Config.local ; deep } =
      List.map2
        (fun field_name
          (module Field : Field
             with type enclosing_a = a
             and type enclosing_param_names = param_names
             and type enclosing_deep_config = deep_config) ->
          let config =
            let config_from_deep =
              option_get
                ~default:{ Config.local = Config.local_zero ; deep = Field.default_deep_config }
                (Field.project_config deep)
            in
            let local =
              let default_local =
                let label = Some [pcdata (default_label_of_field_name field_name)] in
                let default = option_bind Field.project_default local.Config.default in
                let template_data = Field.default_template_data ?default () in
                { Config.label ; default ; template = None ; annotation = None ; template_data }
              in
              Config.option_or_by_field [ config_from_deep.Config.local ; default_local ]
            in
            { config_from_deep with Config.local }
          in
          let content =
            let param_names = Field.project_param_names param_names in
            Field.pre_render false submit param_names config
          in
          Config.({ Field_rendering.content ;
                    label = config.local.label ;
                    annotation = config.local.annotation }))
        Options.field_names
        Options.fields

    let pre_render is_outmost submit param_names config =
      let { Config.label ; annotation ; default ; template ; template_data } = config.Config.local in
      let template = option_get ~default:Options.default_template template in
      template ~is_outmost ?submit ?label ?annotation ?default ~param_names ?template_data
        (field_renderings submit param_names config)

    let func k ?submit =
      Config.local_fun
        (fun local () ->
          Options.opt_field_configs_fun
            (fun deep deep_arg ->
              k submit deep_arg { Config.local ; deep }))
    let content = func (pre_render true)
    let field = func (fun submit () config -> assert (submit = None); config) ?submit:None
    let get_handler f =
      fun repr post ->
        f (from_repr repr) post
    let post_handler f =
      fun get repr ->
        f get (from_repr repr)
  end

module type Atomic_options = sig
  type a
  type param_names
  type template_data
  val default_template_data : ?default:a -> unit -> template_data option
  val default_template : (a, param_names, template_data) template
  val params_type : string -> (a, [`WithoutSuffix], param_names) Eliom_parameter.params_type
end

module Make_atomic_options (Atomic_options : Atomic_options) = struct
  include Atomic_options
  type deep_config = unit
  type repr = a
  let to_repr x = x
  let from_repr x = x
  type ('arg, 'res) opt_field_configs_fun = 'arg -> 'res
  let default_template = default_template
  let fields = []
  let field_names = []
  let default_deep_config = ()
  let opt_field_configs_fun f x = f () x
end

{shared{
let field_not_required_class = "__eliom_form_field_not_required"
let input_marker_class = "__eliom_form_input_marker"
}}
let input_marker =
  Eliom_content.Html5.F.(span ~a:[a_class [input_marker_class]] [])

let form_string_default_template can_be_empty =
  fun ~is_outmost ?submit ?label ?annotation ?default ?(classes=[])
    ?template_data:opt_pattern ~param_names field_renderings ->
      assert (field_renderings = []);
      let open Eliom_content.Html5.F in
      let not_required_class_maybe, required_maybe =
        if can_be_empty then
          [field_not_required_class], []
        else
          [], [a_required `Required]
      in
      let pattern_maybe =
        option_get ~default:[]
          (option_map (fun p -> [a_pattern p])
             opt_pattern)
      in
      let a =
        a_class (not_required_class_maybe @ classes) :: required_maybe @ pattern_maybe
      in [
        string_input ~a ~name:param_names ?value:default ~input_type:`Text ();
        input_marker;
      ]

module Form_string = struct
  module Atomic_options = struct
    type a = string
    type param_names = [`One of string] Eliom_parameter.param_name
    type template_data = string
    let default_template_data ?default () = None
    let params_type = Eliom_parameter.string
    let default_template = form_string_default_template false
  end
  module Options = Make_atomic_options (Atomic_options)
  include Make (Options)
end

module Form_int = struct
  module Options =
    Make_atomic_options
      (struct
        type a = int
        type param_names = [`One of int] Eliom_parameter.param_name
        let params_type = Eliom_parameter.int
        type template_data = unit
        let default_template_data ?default () = None
        let default_template ~is_outmost ?submit ?label ?annotation ?default
            ?(classes=[]) ?template_data ~param_names field_renderings =
          assert (field_renderings = []);
          let open Eliom_content.Html5.F in [
            int_input ~a:[a_required `Required; a_class classes]
              ~name:param_names ?value:default ~input_type:`Number ();
            input_marker;
          ]
       end)
  include Make (Options)
end

module Form_unit = struct
  module Options =
    Make_atomic_options
      (struct
        type a = unit
        type param_names = unit
        type template_data = unit
        let default_template_data ?default () = None
        let params_type _ = Eliom_parameter.unit
        let default_template ~is_outmost ?submit ?label ?annotation ?default ?classes ?template_data ~param_names field_renderings =
          assert (field_renderings = []);
          []
       end)
  include Make (Options)
end


(* module Form_list = struct *)
(*   module Make *)
(*   : functor (Options : Options with type param_names = string Eliom_parameter.setone Eliom_parameter.param_name) -> Form *)
(*   = functor (Options : Options with type param_names = string Eliom_parameter.setone Eliom_parameter.param_name) -> struct *)
(*       module Options = struct *)
(*         type a = Options.a list *)
(*         type repr = Options.repr list *)
(*         type param_names = [`Set of string] Eliom_parameter.param_name *)
(*         let params_type prefix : (repr, [`WithoutSuffix], param_names) Eliom_parameter.params_type = *)
(*           Eliom_parameter.set (Options.params_type : string -> (_, _, string Eliom_parameter.setone) Eliom_parameter.params_type :> string -> (_, _, [`One of string]) Eliom_parameter.params_type) prefix *)
(*         type deep_config = (Options.a, Options.param_names, Options.deep_config) Config.t option *)
(*         let default_deep_config = None *)
(*         let to_repr = List.map Options.to_repr *)
(*         let from_repr = List.map Options.from_repr *)
(*         type ('arg, 'res) opt_field_configs_fun = *)
(*             deep_config -> 'arg -> 'res *)
(*         let opt_field_configs_fun f = f *)
(*         let field_names = [] *)
(*         let fields = [] *)
(*         module Field = Make (struct *)
(*           include (Options : module type of Options with type param_names := Options.param_names) *)
(*           type param_names = [`Set of string] Eliom_parameter.param_name *)
(*           let params_type prefix : (repr, _, param_names) Eliom_parameter.params_type = *)
(*             failwith "Form_list.Field.params_type" *)
(*           let fields = [] *)
(*           let default_template = [] *)
(*             (\* List.map *\) *)
(*             (\*   (fun (module Field : Field with type a =  *\) *)
(*             (\*   (fun (field : (a, Options.param_names, deep_config) field) -> *\) *)
(*             (\*     let module Field = (val field) in *\) *)
(*             (\*     let module Field = struct *\) *)
(*             (\*       include (Field : module type of Field with type enclosing_param_names := Field.enclosing_param_names) *\) *)
(*             (\*       type enclosing_param_names = [`Set of string] Eliom_parameter.param_name *\) *)
(*             (\*       let project_param_names x = x *\) *)
(*             (\*     end in *\) *)
(*             (\*     ((module Field) : (a, param_names, deep_config) field)) *\) *)
(*             (\*   Options.fields *\) *)
(*         end) *)
(*         let default_template : (a, param_names) template = *)
(*           let open Eliom_content.Html5.F in *)
(*           fun ~is_outmost ?submit ?label ?annotation ?default ~param_names field_renderings -> *)
(*             assert (field_renderings = []); *)
(*             let field_renderings = *)
(*               let fields = *)
(*                 match default with *)
(*                   | Some values -> List.map (fun x -> Some x) values *)
(*                   | None -> Array.to_list (Array.make 5 None) *)
(*               in *)
(*               List.map *)
(*                 (fun default -> *)
(*                   let label = None in *)
(*                   let annotation = None in *)
(*                   let template = None in *)
(*                   let local = { Config.label ; annotation ; default ; template } in *)
(*                   let deep = Options.default_deep_config in *)
(*                   let config = { Config.local ; deep } in *)
(*                   let content = Field.pre_render false submit param_names config in *)
(*                   { Field_rendering.label; content; annotation }) *)
(*                 fields *)
(*             in *)
(*             template_table ~is_outmost ~param_names ?submit field_renderings *)
(*       end *)
(*       include Make (Options) *)
(*   end *)
(* end *)

{shared{
  let form_option_class = "__eliom_form_option"
  let form_option_checkbox_class = "__eliom_form_option_checkbox"
}}

{client{
  let rec parent_with_class =
    fun class_ element ->
      Js.Opt.case
        (Js.Opt.bind
           (element ## parentNode)
           Dom_html.CoerceTo.element)
        (fun () ->
          None)
        (fun parent ->
          if Js.to_bool (parent ## classList ## contains (Js.string class_)) then
            Some parent
          else
            parent_with_class class_ parent)

  let is_option_checked form =
    let checkbox =
      Js.Opt.get
        (Js.Opt.bind
           (form ## querySelector
              (Printf.ksprintf
                 (fun x -> Eliom_lib.debug "QUERY: %S" x; Js.string x)
                 " .field:nth-child(1) .%s"
                 form_option_checkbox_class))
           Dom_html.CoerceTo.input)
        (fun () -> Eliom_lib.error "set_checked: No checkbox")
    in
    Js.to_bool (checkbox ## checked)

  let rec is_option_checked_rec form =
    is_option_checked form &&
      (match parent_with_class form_option_class form with
        | None -> true
        | Some parent_form ->
          is_option_checked_rec parent_form)

  let rec set_checked =
    fun form ->
      Firebug.console ## log_2 (Js.string "set_checked", form);
      try
      let inputs =
        form ## querySelectorAll
          (Printf.ksprintf Js.string
             "input:not([type='checkbox']):not(.%s)"
             field_not_required_class)
      in
      List.iter
        (fun input ->
          let checked =
            option_get' ~default:(fun () -> false)
              (option_map
                 (fun form ->
                   is_option_checked_rec form)
                 (parent_with_class form_option_class input))
          in
          let input =
            Js.Opt.get
              (Dom_html.CoerceTo.input input)
              (fun () -> Eliom_lib.error "set_checked: not an input")
          in
          input ## required <- Js.bool checked)
        (Dom.list_of_nodeList inputs);
      option_iter set_checked
        (parent_with_class form_option_class form)
      with exc ->
        Eliom_lib.error "ERROR: %s"
          (Printexc.to_string exc)
}}

module Form_option = struct

  type ('a, 'template_data) option_template_data =
    < template : ('a, unit, 'template_data) template ;
      template_data : 'template_data option ;
      label : form_content option ;
      annotation : form_content option >

  type 'param_names option_param_names =
    [`One of bool] Eliom_parameter.param_name * 'param_names

  let template_data ?(template=default_template) ?template_data ?label ?annotation () =
    object
      method template = template
      method template_data = template_data
      method label = label
      method annotation = annotation
    end

  let field : 'a 'param_names 'deep_config 'template_data .
      ( 'a option,
        'param_names option_param_names,
        ('a, 'param_names, 'deep_config, 'template_data) Config.t option,
        ('a, 'template_data) option_template_data,
        ('a, 'param_names, 'deep_config, 'template_data) Config.t,
        ( 'a option,
          'param_names option_param_names,
          ('a, 'param_names, 'deep_config, 'template_data) Config.t option,
          ('a, 'template_data) option_template_data
        ) Config.t
      ) Config.local_fun
      =
    fun ?label ?annotation ?default ?template ?template_data arg ->
      Config.local_fun
        (fun local deep ->
          { Config.local ; deep = Some deep })
        ?label ?annotation ?default ?template ?template_data arg

  let template : 'a 'param_names 'template_data .
      ( 'a,
        'param_names,
        'template_data
      ) template ->
      ( 'a option,
        'param_names option_param_names,
        ( 'a, 'template_data ) option_template_data
      ) template
    =
    fun the_template ->
    fun ~is_outmost ?submit ?label ?annotation ?default ?(classes=[])
      ?(template_data=template_data ())
      ~param_names:(checkbox_param_name, param_names)
      field_renderings ->
        let set_checked = {Dom_html.element Js.t -> unit{
          fun (checkbox : Dom_html.element Js.t) ->
            match parent_with_class form_option_class checkbox with
            | Some field ->
              set_checked field
            | None -> Eliom_lib.error "Form_option.template: no parent field"
        }} in
        let onclick = {{
          fun ev ->
            let checkbox =
              Js.Optdef.get (ev ## target)
                (fun () -> Eliom_lib.error "Form_option.template: not an input")
            in
            %set_checked checkbox
        }} in
        let field_rendering =
          let checkbox =
            let open Eliom_content.Html5.F in
            let checked = option_get ~default:None default <> None in
            let checkbox =
              let a = [a_onclick onclick; a_class [form_option_checkbox_class]] in
              Eliom_content.Html5.D.bool_checkbox ~a ~checked ~name:checkbox_param_name ()
            in
            ignore {unit{
              Eliom_client.onload
                (fun () ->
                  %set_checked (Eliom_content.Html5.To_dom.of_element %checkbox))
            }};
            checkbox
          in
          let label = Some (option_get ~default:[] template_data#label @ [checkbox]) in
          let content =
            let default = option_get ~default:None default in
            the_template ~is_outmost:false ?submit ?default ~param_names field_renderings
          in
          { Field_rendering.label ; annotation ; content } in
        let classes = form_option_class :: classes in
        let default = option_get ~default:None default in
        template_data#template ~is_outmost ?submit ?label
          ?annotation:template_data#annotation ?default ~classes
          ?template_data:template_data#template_data
          ~param_names:() [field_rendering]

  module Make :
    functor (Options:Options) -> Form
      with type a = Options.a option
      and type repr = bool * Options.repr option
      and type param_names = Options.param_names option_param_names
      and type template_data = (Options.a, Options.template_data) option_template_data
      and type deep_config = (Options.a, Options.param_names, Options.deep_config, Options.template_data) Config.t option
      and type ('arg, 'res) opt_field_configs_fun =
            (Options.a, Options.param_names, Options.deep_config, Options.template_data) Config.t option -> 'arg -> 'res
  =
    functor (Options:Options) -> struct
      module Options = struct
        type a = Options.a option
        type repr = bool * Options.repr option
        type param_names = Options.param_names option_param_names
        type template_data = (Options.a, Options.template_data) option_template_data
        let default_template_data ?default () = None
        let params_type prefix =
          Eliom_parameter.prod
            (Eliom_parameter.bool (prefix^"_is_some"))
            (Eliom_parameter.opt (Options.params_type (prefix^"_some")))
        type deep_config = (Options.a, Options.param_names, Options.deep_config, Options.template_data) Config.t option
        let default_deep_config = None

        let to_repr = function
          | None -> false, None
          | Some x -> true, Some (Options.to_repr x)
        let from_repr = function
          | (false, _) -> None
          | (true, Some x) -> Some (Options.from_repr x)
          | _ -> failwith "Form_option_functor.from_repr"

        type ('arg, 'res) opt_field_configs_fun =
            deep_config -> 'arg -> 'res

        let opt_field_configs_fun f = f

        let field_names = [ "" ]

        let fields =
          let module Field = struct
            type enclosing_a = a
            type enclosing_param_names = param_names
            type enclosing_deep_config = deep_config
            let project_default x = x
            let project_param_names (_, param_names) =
              param_names
            let project_config deep_config = deep_config
            include Make (Options)
          end in
          [ ((module Field) : (a, param_names, deep_config) field) ]

        let default_template : (a, param_names, template_data) template =
          template default_template
      end
      include Make (Options)
    end
end
