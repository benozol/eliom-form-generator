
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

{client{
  let debug = Eliom_lib.debug
  let to_dom = Eliom_content.Html5.To_dom.of_element
}}

{shared{

type div_content = Html5_types.div_content Eliom_content.Html5.elt list
type form_content = Html5_types.form_content Eliom_content_core.Html5.elt list
type form_elt = Html5_types.form_content Eliom_content_core.Html5.elt
type button_content = Html5_types.button_content Eliom_content_core.Html5.elt list
type flow5 = Html5_types.flow5 Eliom_content_core.Html5.elt list
type pcdata = Html5_types.pcdata Eliom_content_core.Html5.elt
let pcdata = Eliom_content.Html5.F.pcdata
let ksprintf = Printf.ksprintf

let option_get ~default = function
  | Some x -> x
  | None -> default
let option_get' ~default = function
  | Some x -> x
  | None -> default ()
let option_map ~f = function
  | Some x -> Some (f x)
  | None -> None
let option_get_map ~default ~f o =
  option_get ~default (option_map ~f o)
let option_iter f = function
  | Some x -> f x
  | None -> ()
let option_to_list = function
  | Some x -> [x]
  | None -> []
let option_bind f = function
  | Some x -> f x
  | None -> None
let maybe_get_option_map really opt f =
  if really then
    option_get ~default:[] (option_map f opt)
  else []
let some x = Some x
let is_some = function Some _ -> true | _ -> false
let from_some = function Some x -> x | _ -> failwith "from_some"
let rec option_or = function
  | [] -> None
  | None :: rest -> option_or rest
  | some :: _ -> some
let list_filter_some li =
  List.map (option_get' ~default:(fun () -> assert false))
    (List.filter (fun x -> x <> None) li)

let identity x = x
let (%) f g x = f (g x)
let (@@) f x = f x

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

(******************************************************************************)

let form_class = "__eliom_form__"
let form_sum_class = "__eliom_form_sum__"
let form_sum_variant_class = "__eliom_form_sum_variant__"
let form_sum_radio_variant_selector_class = "__eliom_form_sum_variant_selector__"
let form_sum_dropdown_variant_selector_class = "__eliom_form_sum_variant_selector__"
let form_sum_radio_class = "__eliom_form_sum_radio__"
let form_sum_dropdown_class = "__eliom_form_sum_dropdown__"
let form_record_class = "__eliom_form_record__"
let form_option_class = "__eliom_form_option__"
let form_option_checkbox_class = "__eliom_form_option_checkbox__"
let component_not_required_class = "__eliom_form_component_not_required__"
let input_marker_class = "__eliom_form_input_marker__"

let prefix_concat ~prefix suffix = prefix ^ "|" ^ suffix
let param_name_root = "__eliom_form__"

let form_sum_variant_attribute =
  Eliom_content.Html5.Custom_data.create
    ~name:"__eliom_form_sum_variant"
    ~to_string:identity ~of_string:identity ()

module Component_rendering = struct
  type t = {
    label : form_content option;
    selector : form_content option;
    content : form_content;
    annotation : form_content option;
    a : Html5_types.div_attrib Eliom_content.Html5.F.attrib list option;
  }
  let mk ?label ?selector ?annotation ?a ~content () =
    { label ; selector ; annotation ; a ; content }
end

type 'param_names or_display = [ `Display | `Param_names of string * 'param_names ]

module Template = struct

  type ('a, 'param_names, 'template_data) arguments = {
    is_outmost : bool ;
    submit : button_content option ;
    label : form_content option ;
    annotation : form_content option ;
    default : 'a option ;
    a : Html5_types.div_attrib Eliom_content.Html5.F.attrib list option;
    template_data : 'template_data ;
    param_names : 'param_names or_display ;
    component_renderings : Component_rendering.t list ;
  }

  type ('a, 'param_names, 'template_data) t =
    ('a, 'param_names, 'template_data) arguments -> form_content

  let arguments
      ~is_outmost ?submit ?label ?annotation ?default ?a
      ~template_data ~param_names component_renderings =
    { is_outmost ; submit ; label ; annotation ; default ; a ;
      template_data ; param_names ; component_renderings }

  let template f =
    fun { is_outmost ; submit ; label ; annotation ; default ; a ;
          template_data ; param_names ; component_renderings } ->
      f ~is_outmost ?submit ?label ?annotation ?default ?a
        ~template_data ~param_names component_renderings
end

let template_concat : (_, _, _) Template.t =
  fun arguments ->
    Template.template
      (let open Eliom_content.Html5.F in
       fun ~is_outmost ?submit ?label ?annotation ?default ?(a=[]) ~template_data
         ~param_names component_renderings ->
           [ div ~a:(a_class [form_class] :: a)
               (List.map
                  (fun { Component_rendering.label ; selector ; content ; annotation ; a } ->
                    div ?a [
                      div (option_get ~default:[] selector) ;
                      div (option_get ~default:[] label) ;
                      div content ;
                      div (option_get ~default:[] annotation) ;
                    ])
                  component_renderings) ])
      arguments

let template_table =
  fun arguments ->
    Template.template
      (let open Eliom_content.Html5.F in
       fun ~is_outmost ?submit ?label ?annotation ?default ?(a=[]) ~template_data
         ~param_names field_renderings ->
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
             List.map from_some @@
             List.filter is_some @@
             List.map
               (fun { Component_rendering.label ; selector ; content ; annotation ; a } ->
                 if selector = None && annotation = None && content = [] then
                   None
                 else
                   let a = option_get ~default:[] a in
                   let label =
                     td ~a:[a_class ["label"]]
                       (option_get ~default:[] label)
                   in
                   let selector =
                     option_to_list
                       (option_map
                          (td ~a:[a_class ["selector"]])
                          selector)
                   in
                   let content =
                     td ~a:[a_class ["content"]] content
                   in
                   let annotation =
                     option_to_list
                       (option_map
                          (td ~a:[a_class ["annotation"]])
                          annotation)
                   in
                   Some (tr ~a:(a_class ["field"] :: a)
                           (selector @ label :: content :: annotation)))
               field_renderings
           in
           let contents = captions @ fields @ annotations @ submits in
           let outmost_class = if is_outmost then ["outmost"] else [] in
           match contents with
           | [] -> []
           | hd :: tl ->
             [ table ~a:(a_class (["form"; form_class] @ outmost_class) :: a)
                 hd tl ])
      arguments

let default_template =
  template_table

let default_label_of_component_name s =
  let s =
    if Str.string_match (Str.regexp "[a-zA-Z]_+") s 0
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
    template : ('a, 'param_names, 'template_data) Template.t option;
    template_data : 'template_data option;
    classes : string list option;
  }
  let local_zero = {
    label = None; annotation = None; default = None;
    template = None ; template_data = None; classes = None;
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
           classes = option_or [c1.classes; c2.classes];
         } :: rest)
  type ('a, 'param_names, 'deep_config, 'template_data) t = {
    local : ('a, 'param_names, 'deep_config, 'template_data) local;
    deep : 'deep_config;
  }
  type ('a, 'param_names, 'deep_config, 'template_data, 'arg, 'res) local_fun =
    ?label:form_content ->
    ?annotation:form_content ->
    ?default:'a ->
    ?classes:string list ->
    ?template:('a, 'param_names, 'template_data) Template.t ->
    ?template_data:'template_data ->
    'arg -> 'res
  let local_fun k ?label ?annotation ?default ?classes ?template ?template_data arg =
    k { label ; default ; annotation ; template ; template_data ; classes } arg
end

(******************************************************************************)

module type Repr = sig
  type t
  type repr
  val of_repr : repr -> t
  val to_repr : t -> repr
end

module type Template_data = sig
  type template_data
  type 'res template_data_fun
  val pre_template_data : (template_data -> 'res) -> 'res template_data_fun
  val apply_template_data_fun : 'res template_data_fun -> 'res
end

module type Base_options = sig
  type a
  type param_names
  type deep_config
  type ('arg, 'res) opt_component_configs_fun
  include Repr with type t := a
  include Template_data
  val params_type' : string -> string * (repr, [`WithoutSuffix], param_names) Eliom_parameter.params_type
  val opt_component_configs_fun : (deep_config -> 'arg -> 'res) ->
    ('arg, 'res) opt_component_configs_fun
  val default_deep_config : deep_config
  val default_template : (a, param_names, template_data) Template.t
  val component_names : string list
end

module type Pre_form = sig
  include Base_options
  val pre_render : bool -> button_content option -> param_names or_display ->
    (a, param_names, deep_config, template_data) Config.t -> form_content
end

module type Form = sig
  include Pre_form
  type config = (a, param_names, deep_config, template_data) Config.t
  val params_type : string -> (repr, [`WithoutSuffix], param_names) Eliom_parameter.params_type
  val template_data : template_data template_data_fun
  val content :
    ?submit:button_content ->
    ( a, param_names, deep_config, template_data, unit,
      (unit, param_names -> form_content) opt_component_configs_fun
    ) Config.local_fun
  val display :
    value:a ->
    ( a, param_names, deep_config, template_data, unit,
      (unit, form_content) opt_component_configs_fun ) Config.local_fun
  val config :
    ( a, param_names, deep_config, template_data, unit,
      (unit, config) opt_component_configs_fun ) Config.local_fun
  val get_handler : (a -> 'post -> 'res) -> (repr -> 'post -> 'res)
  val post_handler : ('get -> a -> 'res) -> ('get -> repr -> 'res)
end

(******************************************************************************)

module type Field = sig
  include Pre_form
  type enclosing_a
  type enclosing_param_names
  type enclosing_deep_config
  val project_default : enclosing_a -> a option
  val project_param_names : enclosing_param_names -> param_names
  val project_config : enclosing_deep_config ->
    (a, param_names, deep_config, template_data) Config.t option
  val prefix : string -> string
end

type ('a, 'param_names, 'deep_config) field =
  (module Field with
    type enclosing_a = 'a and
    type enclosing_param_names = 'param_names and
    type enclosing_deep_config = 'deep_config)

module type Record_options = sig
  include Base_options
  val fields : (a, param_names, deep_config) field list
end

(******************************************************************************)

type selector_param_name = [`One of string] Eliom_parameter.param_name

module type Variant = sig
  include Field
  val is_constructor : enclosing_a -> bool
end

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

(******************************************************************************)

module Make_base (Options : Base_options) = struct
  include Options
  let params_type prefix = snd (params_type' (prefix^param_name_root))
  let template_data = pre_template_data identity
  type config = (a, param_names, deep_config, template_data) Config.t
  let get_handler f =
    fun repr post ->
      f (of_repr repr) post
  let post_handler f =
    fun get repr ->
      f get (of_repr repr)
end
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

  let find_form_node node =
    match parent_with_class form_class (node :> Dom_html.element Js.t) with
      | Some form_node -> form_node
      | None -> failwith "find_form_node"

  (* let get_form_sum_variant_attribute, set_form_sum_variant_attribute, unset_form_sum_variant_attribute = *)
  (*   let get (form_node : #Dom_html.element Js.t) = *)
  (*     Js.Opt.case (form_node ## getAttribute (attribute)) *)
  (*       (fun () -> None) *)
  (*       (fun str -> Some (Js.to_string str)) *)
  (*   in *)
  (*   let set (form_node : #Dom_html.element Js.t) variant_name = *)
  (*     form_node ## setAttribute (attribute, Js.string variant_name) *)
  (*   in *)
  (*   let unset (form_node : Dom_html.element Js.t) = *)
  (*     form_node ## removeAttribute (attribute) *)
  (*   in *)
  (*   get, set, unset *)

  let classify_form_node form_node =
    let form_node = (form_node :> Dom_html.element Js.t) in
    let contains clazz = Js.to_bool (form_node ## classList ## contains (Js.string clazz)) in
    if not (contains form_class) then
      failwith "classify_form_node";
    match contains form_record_class, contains form_sum_class, contains form_option_class with
      | true, false, false -> `Record
      | false, true, false ->
        begin match contains form_sum_radio_class, contains form_sum_dropdown_class with
          (* | true, false -> `Sum `Radio *)
          | false, true -> `Sum `Drop_down
          | _ -> failwith "classify_form_node: sum"
        end
      (* | false, false, true -> `Option *)
      | _ -> failwith "classify_form_node"

  let rec nodes_between ~root ~descendent =
    Js.Opt.case (descendent ## parentNode)
      (fun () -> failwith "nodes_between")
      (fun parent ->
        if parent == root then
          []
        else
          parent :: nodes_between ~root ~descendent:parent)

  (* let local_to_form ~form_node nodes = *)
  (*   List.filter *)
  (*     (fun node -> *)
  (*       List.for_all *)
  (*         (fun between -> note (between ## classes ## contains (form_class))) *)
  (*         (nodes_between ~root ~descendent:node)) *)
  (*     nodes *)

  (* let form_sum_current_variant form_node radio_or_dropdow = *)
  (*   match radio_or_dropdow with *)
  (*     | `Radio -> *)
  (*         let variants = *)
  (*           let sum_variant_class_str = ksprintf Js.string ".%s" form_sum_variant_class in *)
  (*           local_to_form ~form_node *)
  (*             (form_node ## querySelectorAll (sum_variant_class_str)) *)
  (*         in *)
  (*         begin match *)
  (*           local_to_form ~form_node *)
  (*             (form_node ## querySelectorAll (ksprintf Js.string ".%:checked" form_sum_radio_variant_selector_class)) *)
  (*         with *)
  (*           | [] -> None *)
  (*           | [input_node] -> *)
  (*             let variant_node = parent_with_class form_sum_variant_class input_node in *)
  (*             Js.Opt.get *)
  (*               (variant_node ## attributes ## getAttribute (Js.string form_sum_variant_attribute)) *)
  (*               (fun () -> faiwith "form_sum_current_variant: no variant attribute") *)
  (*           | _ -> failwith "form_sum_current_variant: multiple selected variants" *)
  (*         end *)
  (*     | `Drop_down -> *)
  (*         let selector = *)
  (*           match *)
  (*             local_to_form ~form_node *)
  (*               (form_node ## querySelectorAll (ksprintf Js.string ".%s" form_sum_dropdown_variant_selector_class)) *)
  (*           with *)
  (*             | [selector] -> selector *)
  (*             | _ -> failwith "form_sum_current_variant: to many or no variant selectors" *)
  (*         in *)
  (*         if selector ## value = "" then *)
  (*           None *)
  (*         else Some selector ## value *)


  let rec is_required_rec : Dom_html.element Js.t -> bool =
    fun node ->
      match parent_with_class form_class node with
      | None -> true
      | Some form_node ->
        let locally_required =
          match classify_form_node form_node with
          | `Record -> true
          | `Sum `Drop_down ->
              if Js.to_bool (node ## classList ## contains
                   (Js.string form_sum_dropdown_variant_selector_class)) then
                true
              else
                begin
                  try
                    let selected_variant_name =
                      Eliom_content.Html5.Custom_data.get_dom form_node form_sum_variant_attribute
                    in
                    let variant_node =
                      option_get' ~default:(fun () -> failwith "is_required_rec: variant")
                        (parent_with_class form_sum_variant_class node) in
                    let variant_name =
                      Eliom_content.Html5.Custom_data.get_dom variant_node
                        form_sum_variant_attribute
                    in
                    variant_name = selected_variant_name
                  with Not_found -> false
                end
        in
        locally_required && is_required_rec form_node
  let is_required_rec node = is_required_rec (node :> Dom_html.element Js.t)

  let form_inputs_set_required form_node =
    let inputs =
      form_node ## querySelectorAll
        (Printf.ksprintf Js.string
           "input:not([type='checkbox']):not(.%s),\
            select:not(.%s)"
           component_not_required_class
           component_not_required_class)
    in
    List.iter
      (fun node ->
        Js.Opt.iter
          (Dom_html.CoerceTo.input node)
          (fun input ->
            input ## required <- Js.bool (is_required_rec input));
        Js.Opt.iter
          (Dom_html.CoerceTo.select node)
          (fun select ->
            select ## required <- Js.bool (is_required_rec select)))
      (Dom.list_of_nodeList inputs)

  let connect_select_variant select_node =
    Lwt_js_events.async
      (fun () ->
        Lwt_js_events.changes select_node
          (fun _ _ ->
            let form_node = find_form_node (select_node :> Dom_html.element Js.t) in
            let variant_name = Js.to_string (select_node ## value) in
            Eliom_content.Html5.Custom_data.set_dom form_node
              form_sum_variant_attribute variant_name;
            form_inputs_set_required form_node;
            Lwt.return ()))
}}

{shared{

let for_outmost ~is_outmost = function
  | elt :: elts when is_outmost ->
    let id = Eliom_content.Html5.Id.new_elt_id () in
    ignore {unit{
      Eliom_client.onload
      (fun () ->
        form_inputs_set_required
          (Eliom_content.Html5.To_dom.of_element
             (Eliom_content.Html5.Id.get_element %id)))
    }};
    Eliom_content.Html5.Id.create_named_elt ~id elt :: elts
  | elts -> elts

(******************************************************************************)

type variant_selection = [`Drop_down(* | `Radio*)]
let default_variant_selection : variant_selection = `Drop_down

type 'template_data sum_template_data =
  variant_selection * 'template_data
type 'template_data_fun sum_template_data_fun =
  ?variant_selection:variant_selection -> unit -> 'template_data_fun

module Make_sum
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
          ( Options.a, Options.param_names, Options.deep_config,
            variant_selection * Options.template_data
          ) Config.t and
        type ('arg, 'res) opt_component_configs_fun =
          ('arg, 'res) Options.opt_component_configs_fun
= functor (Options : Sum_options) -> struct

  let template_arguments_to_options :
      (_, _, Options.template_data sum_template_data) Template.arguments ->
      (_, _, Options.template_data) Template.arguments =
    fun { Template.is_outmost; submit; label; annotation; default;
          a; template_data = (variant_selection, template_data);
          param_names; component_renderings } ->
      let a =
        let variant_selection_class =
          match variant_selection with
          | `Drop_down -> form_sum_dropdown_class
          (* | `Radio -> form_sum_radio_class *)
        in
        let classes =[form_class; form_sum_class; variant_selection_class] in
        Some (Eliom_content.Html5.F.a_class classes :: option_get ~default:[] a)
      in
      { Template.is_outmost; submit; label; annotation; default;
        a; param_names; component_renderings; template_data }

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
    let pre_template_data : (template_data -> 'res) -> 'res template_data_fun =
      fun (k : template_data  -> _) ?(variant_selection=default_variant_selection) () ->
        Options.pre_template_data (fun data -> k (variant_selection, data))
    let apply_template_data_fun (f : _ template_data_fun) =
        Options.apply_template_data_fun (f ())
    let default_template =
      fun args ->
        Options.default_template (template_arguments_to_options args)
  end

  include Make_base (Options')

  let config =
    Config.local_fun
      (fun local () ->
        Options.opt_component_configs_fun
          (fun deep () ->
            { Config.local ; deep }))

  let variant_renderings submit (param_names : param_names or_display)
      { Config.local ; deep } variant_selection =
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
                            local.Config.default
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
                        connect_select_variant select_node;
                        Lwt.return ())
                  }};
                  div ~a:[a_class ["contains_select"]] [ select ]
              | `Display ->
                  let variant_name =
                    match local.Config.default with
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
                 ~default:{ Config.local = Config.local_zero ;
                            deep = Variant.default_deep_config }
                 (Variant.project_config deep)
             in
             let local =
               let default_local =
                 let label = Some [pcdata (default_label_of_component_name variant_name)] in
                 let default = option_bind Variant.project_default local.Config.default in
                 let template_data =
                   Some (Variant.apply_template_data_fun (Variant.pre_template_data identity))
                 in
                 { Config.label ; default ;
                   template = None ; annotation = None ;
                   template_data ; classes = None }
               in
               Config.option_or_by_field [ config_from_deep.Config.local ; default_local ]
             in
             { config_from_deep with Config.local }
           in
           let is_constructor =
             option_get_map ~default:false ~f:Variant.is_constructor local.Config.default
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
                     ?label:Config.(config.local.label)
                     ?annotation:Config.(config.local.annotation)
                     ~content ()))
         Options.component_names
         Options.variants)


  let pre_render is_outmost submit param_names config =
    let { Config.label ; annotation ; default ;
          template ; template_data = template_data_opt } =
      config.Config.local in
    let variant_selection, _ as template_data =
      option_get' ~default:(fun () -> apply_template_data_fun (pre_template_data identity)) template_data_opt
    in
    let template =
      option_get
        ~default:(fun args ->
          Options.default_template
            (template_arguments_to_options args))
        template
    in
    let component_renderings = variant_renderings submit param_names config variant_selection in
    for_outmost ~is_outmost
      (template
         (Template.arguments
            ~is_outmost ?submit ?label ?annotation ?default
            ~param_names ~template_data component_renderings))


  let content ?submit : (_, _, _, _, _, _) Config.local_fun =
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
            pre_render true None `Display { Config.local ; deep }))

end

(******************************************************************************)

module Make_record :
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
      for_outmost ~is_outmost
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

(******************************************************************************)

module Template_data_unit :
  Template_data with
    type template_data = unit and
    type 'res template_data_fun = 'res =
struct
  type template_data = unit
  type 'res template_data_fun = 'res
  let apply_template_data_fun (f : _ template_data_fun) = f
  let pre_template_data k = k ()
end

module type Atomic_options = sig
  type a
  type param_names
  include Template_data
  val default_template : (a, param_names, template_data) Template.t
  val params_type : string -> (a, [`WithoutSuffix], param_names) Eliom_parameter.params_type
end

module Make_atomic_options (Atomic_options : Atomic_options) = struct
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

let input_marker =
  Eliom_content.Html5.F.(span ~a:[a_class [input_marker_class]] [])

let form_string_default_template can_be_empty =
  let open Eliom_content.Html5.F in
  Template.template
    (fun ~is_outmost ?submit ?label ?annotation ?default ?(a=[])
      ~template_data:opt_pattern ~param_names component_renderings ->
      assert (component_renderings = []);
      let not_required_class_maybe, required_maybe =
        if can_be_empty then
          [component_not_required_class], []
        else
          [], [a_required `Required]
      in
      let pattern_maybe =
        option_get ~default:[]
          (option_map (fun p -> [a_pattern p])
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
        | `Display -> [ pcdata (option_get ~default:"" default) ])

module Form_string = struct
  module Atomic_options = struct
    type a = string
    type param_names = [`One of string] Eliom_parameter.param_name
    type template_data = string option
    type 'res template_data_fun = ?required_pattern:string -> unit -> 'res
    let pre_template_data k ?required_pattern () = k required_pattern
    let apply_template_data_fun (f : _ template_data_fun) = f ()
    let params_type = Eliom_parameter.string
    let default_template = form_string_default_template false
  end
  module Options = Make_atomic_options (Atomic_options)
  include Make_record (Options)
end

module Form_int = struct
  module Options =
    Make_atomic_options
      (struct
        type a = int
        type param_names = [`One of int] Eliom_parameter.param_name
        let params_type = Eliom_parameter.int
        include Template_data_unit
        let default_template =
          let open Eliom_content.Html5.F in
          Template.template
            (fun ~is_outmost ?submit ?label ?annotation ?default
              ?(a=[]) ~template_data ~param_names component_renderings ->
                assert (component_renderings = []);
                let a = (a :> Html5_types.input_attrib Eliom_content.Html5.F.attrib list) in
                match param_names with
                  | `Param_names (_, param_names) -> [
                    int_input ~a:(a_required `Required :: a)
                      ~name:param_names ?value:default ~input_type:`Number ();
                    input_marker;
                  ]
                  | `Display ->
                    [ pcdata (option_get ~default:"" (option_map string_of_int default))])
       end)
  include Make_record (Options)
end

module Form_int64 = struct
  module Options =
    Make_atomic_options
      (struct
        type a = int64
        type param_names = [`One of int64] Eliom_parameter.param_name
        let params_type = Eliom_parameter.int64
        type template_data = (int64 * pcdata * bool) list option
        type 'res template_data_fun = ?from_list:(int64 * pcdata * bool) list -> unit -> 'res
        let pre_template_data k ?from_list () = k from_list
        let apply_template_data_fun (f : _ template_data_fun) = f ()
        let default_template =
          let open Eliom_content.Html5.F in
          Template.template
            (fun ~is_outmost ?submit ?label ?annotation ?default
              ?(a=[]) ~template_data:values_opt ~param_names component_renderings ->
          assert (component_renderings = []);
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
            | Some values when values <> [] ->
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
            | Some values ->
              let a' = (a :> Html5_types.select_attrib Eliom_content.Html5.F.attrib list) in
              let open Eliom_content.Html5.F.Raw in
              [ select ~a:(a_required `Required :: a')
                  [option ~a:[a_value ""] (pcdata required_label)] ]
          end
                | `Display -> [pcdata (option_get ~default:"" (option_map Int64.to_string default))])
       end)
  include Make_record (Options)
end

module Form_unit = struct
  module Options =
    Make_atomic_options
      (struct
        type a = unit
        type param_names = unit
        include Template_data_unit
        let params_type _ = Eliom_parameter.unit
        let default_template config =
          assert (config.Template.component_renderings = []);
          []
       end)
  include Make_record (Options)
end

(******************************************************************************)

}}


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
(*         let of_repr = List.map Options.of_repr *)
(*         type ('arg, 'res) opt_component_configs_fun = *)
(*             deep_config -> 'arg -> 'res *)
(*         let opt_component_configs_fun f = f *)
(*         let component_names = [] *)
(*         let components = [] *)
(*         module Component = Make (struct *)
(*           include (Options : module type of Options with type param_names := Options.param_names) *)
(*           type param_names = [`Set of string] Eliom_parameter.param_name *)
(*           let params_type prefix : (repr, _, param_names) Eliom_parameter.params_type = *)
(*             failwith "Form_list.Component.params_type" *)
(*           let components = [] *)
(*           let default_template = [] *)
(*             (\* List.map *\) *)
(*             (\*   (fun (module Component : Component with type a =  *\) *)
(*             (\*   (fun (component : (a, Options.param_names, deep_config) component) -> *\) *)
(*             (\*     let module Component = (val component) in *\) *)
(*             (\*     let module Component = struct *\) *)
(*             (\*       include (Component : module type of Component with type enclosing_param_names := Component.enclosing_param_names) *\) *)
(*             (\*       type enclosing_param_names = [`Set of string] Eliom_parameter.param_name *\) *)
(*             (\*       let project_param_names x = x *\) *)
(*             (\*     end in *\) *)
(*             (\*     ((module Component) : (a, param_names, deep_config) component)) *\) *)
(*             (\*   Options.components *\) *)
(*         end) *)
(*         let default_template : (a, param_names) template = *)
(*           let open Eliom_content.Html5.F in *)
(*           fun ~is_outmost ?submit ?label ?annotation ?default ~param_names component_renderings -> *)
(*             assert (component_renderings = []); *)
(*             let component_renderings = *)
(*               let components = *)
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
(*                   let content = Component.pre_render false submit param_names config in *)
(*                   { Component_rendering.label; content; annotation }) *)
(*                 components *)
(*             in *)
(*             template_table ~is_outmost ~param_names ?submit component_renderings *)
(*       end *)
(*       include Make (Options) *)
(*   end *)
(* end *)

{shared{
}}

{client{

  let is_option_checked form =
    let checkbox =
      Js.Opt.get
        (Js.Opt.bind
           (form ## querySelector
              (Printf.ksprintf Js.string
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
      try
      let inputs =
        form ## querySelectorAll
          (Printf.ksprintf Js.string
             "input:not([type='checkbox']):not(.%s)"
             component_not_required_class)
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

{shared{
module Form_option = struct
  module Make (Form : Form) = struct
    module Options = struct
      type a = Form.a option
      module Component_None = Form_unit
      module Component_Some = Form
      type param_names =
        [`One of string] Eliom_parameter.param_name *
          (Component_None.param_names * Component_Some.param_names)
      type deep_config = Component_None.config option * Component_Some.config option
      let default_deep_config = None, None
      include Template_data_unit
      type repr = string * (Component_None.repr option * Component_Some.repr option)
      let component_names = [ "None" ; "Some" ]
      let of_repr = function
        | ("None", (Some _, _)) -> None
        | ("Some", (_, Some component)) ->
            Some (Component_Some.of_repr component)
        | _ -> failwith "Form_option: of_repr"
      let to_repr =
        function
        | None -> ("None", ((Some (Component_None.to_repr ())), None))
        | Some component ->
          ("Some", (None, (Some (Component_Some.to_repr component))))
      let prefix_None prefix = prefix
      let prefix_Some prefix = prefix
      let params_type' prefix =
        prefix,
        Eliom_parameter.prod
          (Eliom_parameter.string (prefix ^ "||constructor"))
          (Eliom_parameter.prod
             (Eliom_parameter.opt
                (snd (Component_None.params_type' (prefix_None prefix))))
             (Eliom_parameter.opt
                (snd (Component_Some.params_type' (prefix_Some prefix)))))
      type ('arg, 'res) opt_component_configs_fun =
        ?none: Component_None.config ->
        ?some: Component_Some.config -> 'arg -> 'res
      let opt_component_configs_fun k ?none ?some arg = k (none, some) arg
      let default_template = default_template
      let variants : ((a, param_names, deep_config) variant) list =
        let project_None (none, some) = none
        and project_Some (none, some) = some
        in
        let module Component_None = struct
          type enclosing_a = a
          type enclosing_param_names = param_names
          type enclosing_deep_config = deep_config
          let project_default =
            function | None -> Some () | Some _ -> None
          let _ = project_default
          let project_param_names param_names =
            project_None (snd param_names)
          let _ = project_param_names
          let project_config = project_None
          let _ = project_config
          let is_constructor =
            function | None -> true | Some _ -> false
          let prefix = prefix_None
          include Component_None
        end in
        let module Component_Some = struct
          type enclosing_a = a
          type enclosing_param_names = param_names
          type enclosing_deep_config = deep_config
          let project_default =
            function
            | None -> None
            | Some component -> Some component
          let _ = project_default
          let project_param_names param_names =
            project_Some (snd param_names)
          let _ = project_param_names
          let project_config = project_Some
          let _ = project_config
          let is_constructor =
            function | None -> false | Some _ -> true
          let prefix = prefix_Some
          include Component_Some
        end in
        [ (module Component_None); (module Component_Some) ]
      let project_selector_param_name = fst
    end
    include Make_sum (Options)
  end
end
}}

{shared{
  let form_list_list_class = "__eliom_form_list_list"
  let form_list_list_item_class = "__eliom_form_list_list_item"
  let form_list_remove_button_class = "__eliom_form_list_remove_button"
  let list_suffix = "list"
  let list_elt_suffix = "elt"
  let list_combined_suffix = Printf.sprintf "%s.%s" list_suffix list_elt_suffix
  let param_name_format prefix =
    let const =
      prefix_concat prefix list_combined_suffix
    in
    Scanf.format_from_string const ""^^"%s[%d]%s"
}}
{client{
  type has_name = < name : Js.js_string Js.t Js.prop > Js.t
  let rearrange_input_names prefix ?new_item list_node =
    let set_input_names new_index node =
      Firebug.console ## log_2 (Js.string "set_input_names", node);
      let node = (node :> has_name) in
      let const = prefix_concat prefix list_combined_suffix in
      (* eliom_prefix ^_0 const ^_1 between ^_2 [N ^_3 ] ^ rest *)
      let name = node ## name in
      let index_0 = name ## indexOf (Js.string param_name_root) in
      let index_1 = name ## indexOf_from (Js.string const, index_0) in
      let index_2 = name ## indexOf_from (Js.string "[", index_1) in
      let index_3 = name ## indexOf_from (Js.string "]", index_2) in
      if index_0 >= 0 && index_1 >= 0 && index_2 >= 0 && index_3 >= 0 then
        let eliom_prefix = Js.to_string (name ## substring (0, index_0)) in
        let const = Js.to_string (name ## substring (index_0, index_1)) in
        let between = Js.to_string (name ## substring (index_1, index_2)) in
        let index = Js.to_string (name ## substring (succ index_2, index_3)) in
        let rest = Js.to_string (name ## substring_toEnd (succ index_3)) in
        let new_index =
          string_of_int
            (new_index
               (int_of_string index))
        in
        let new_name = eliom_prefix ^ const ^ between ^ "[" ^ new_index ^ "]" ^ rest in
        node ## name <- Js.string new_name
      else debug "%d %d %d %d" index_0 index_1 index_2 index_3
    in
    let counter = ref 0 in
    let new_index =
      let mapping = Hashtbl.create 7 in
      fun index ->
        debug "new_index %d" index;
        if not (Hashtbl.mem mapping index) then
          (debug "not found for %d" index;
           Hashtbl.add mapping index !counter;
           incr counter);
        Hashtbl.find mapping index
    in
    let input_or_select node =
      let opt = Dom_html.CoerceTo.input node in
      if Js.Opt.test opt then
        (opt :> has_name Js.opt)
      else
        (Dom_html.CoerceTo.select node :> has_name Js.opt)
    in
    List.iter
      (fun node ->
        Js.Opt.iter (input_or_select node)
          (set_input_names new_index))
      (Dom.list_of_nodeList
         (list_node ## querySelectorAll
             (Js.string "input,select")));
    debug "counter: %d" !counter;
    option_iter
      (fun new_item ->
        Js.Opt.iter
          (Dom_html.CoerceTo.element new_item)
          (fun new_item ->
            Dom.appendChild list_node new_item;
            List.iter
              (fun node ->
                Js.Opt.iter (input_or_select node)
                  (set_input_names
                     (function
                       | 0 -> !counter
                       | _ -> failwith "rearrange_input_names: not zero")))
              (Dom.list_of_nodeList
                 (new_item ## querySelectorAll
                    (Js.string "input,select")))))
      new_item;
    ()
  let connect_remove =
    fun prefix a_node ->
      Lwt_js_events.async
        (fun () ->
          lwt ev = Lwt_js_events.click a_node in
          begin
            match
              parent_with_class form_list_list_item_class a_node,
              parent_with_class form_list_list_class a_node
            with
              | Some li_node, Some list_node ->
                Dom.removeChild list_node li_node;
                rearrange_input_names prefix list_node;
                form_inputs_set_required (find_form_node a_node)
              | x,y ->
                failwith "connect_remove: no list/item"
          end;
        Lwt.return ())
}}
{shared{
module Form_list = struct
  type 'repr list_param_names = 'repr Eliom_parameter.listnames
  module Make :
    functor (Form : Form) ->
      Form with
        type a = Form.a list and
        type repr = Form.repr list and
        type param_names = Form.param_names list_param_names and
        type deep_config = Form.config option and
        type template_data = unit and
        type 'res template_data_fun = 'res and
        type config =
          ( Form.a list, Form.param_names list_param_names,
            Form.config option, unit
          ) Config.t and
        type ('arg, 'res) opt_component_configs_fun =
          ?elt:Form.config -> 'arg -> 'res =
    functor (Form : Form) -> struct
      type a = Form.a list
      type repr = Form.repr list
      type param_names = Form.param_names list_param_names
      type deep_config = Form.config option
      let default_deep_config = None
      include Template_data_unit
      let template_data = ()
      let prefix_elt prefix = prefix_concat ~prefix list_suffix
      let params_type' prefix =
        prefix,
        Eliom_parameter.list
          (prefix_elt prefix)
          (snd (Form.params_type' list_elt_suffix))
      let params_type prefix =
        snd (params_type' (prefix^param_name_root))
      type config = (a, param_names, deep_config, template_data) Config.t
      let component_names = []
      let pre_render : bool -> button_content option -> param_names or_display -> config -> form_content =
        let sub_config config sub_default_opt =
          let config =
            let default_config =
              { Config.local = Config.local_zero ;
                deep = Form.default_deep_config }
            in
            option_get ~default:default_config
              config.Config.deep
          in
          { config with
            Config.local = { config.Config.local with
              Config.default =
                option_or [
                  sub_default_opt;
                  config.Config.local.Config.default
                ] } }
        in
        fun is_outmost submit param_names config ->
        match param_names with
          | `Display ->
            begin match config.Config.local.Config.default with
              | None -> []
              | Some default ->
                  let open Eliom_content.Html5.F in
                  let for_sub_default sub_default =
                    li (Form.pre_render false submit `Display
                          (sub_config config (Some sub_default)))
                  in
                  [ ul (List.map for_sub_default default) ]
            end
          | `Param_names (prefix, param_names) ->
            let open Eliom_content.Html5.F in
            let list_item ~dom_semantics li_content =
              let open Eliom_content.Html5.F in
              let remove_a = Eliom_content.Html5.D.Raw.a
                ~a:[a_class [form_list_remove_button_class]]
                [pcdata "Remove"] in
              let li =
                if dom_semantics then
                  Eliom_content.Html5.D.li
                else
                  Eliom_content.Html5.F.li
              in
              li ~a:[a_class [form_list_list_item_class]]
                (li_content @ [ remove_a ]),
              remove_a
            in
            let list =
              Eliom_content.Html5.D.ul
                ~a:[a_class [form_list_list_class]]
                (param_names.Eliom_parameter.it
                   (fun param_names sub_default sofar ->
                     let li, remove_a =
                       list_item ~dom_semantics:true
                         (Form.pre_render false submit
                            (`Param_names (prefix, param_names))
                            (sub_config config (Some sub_default)))
                     in
                     ignore {unit{ connect_remove %prefix (to_dom %remove_a) }};
                     li :: sofar)
                   (option_get ~default:[] config.Config.local.Config.default)
                   [])
            in
            let add_a = Eliom_content.Html5.D.Raw.a [pcdata "Add"] in
            let li_template =
              fst @@ param_names.Eliom_parameter.it
                (fun param_names _ _ ->
                  list_item ~dom_semantics:true
                    (Form.pre_render false submit
                       (`Param_names (prefix, param_names))
                       (sub_config config None)))
                [Obj.magic ()]
                (Obj.magic ())
            in
            ignore {unit{
              Lwt_js_events.async
                (fun () ->
                  Lwt_js_events.clicks
                    (Eliom_content.Html5.To_dom.of_element %add_a)
                    (fun _ _ ->
                      let fresh_li =
                        Js.Opt.get
                          (Dom_html.CoerceTo.element ((to_dom %li_template) ## cloneNode (Js._true)))
                          (fun () -> failwith "fresh_li")
                      in
                      List.iter
                        (fun node ->
                          Js.Opt.iter
                            (Dom_html.CoerceTo.select node)
                            connect_select_variant)
                        (Dom.list_of_nodeList
                           (fresh_li ## querySelectorAll
                              (ksprintf Js.string ".%s"
                                 form_sum_dropdown_variant_selector_class)));
                      let a_node =
                        match
                          Dom.list_of_nodeList @@
                          fresh_li ## querySelectorAll
                                 (ksprintf Js.string ".%s" form_list_remove_button_class)
                        with
                          | [ a_node ] -> a_node
                          | nodes -> failwith "add: remove button"
                      in
                      connect_remove %prefix a_node;
                      rearrange_input_names %prefix ~new_item:fresh_li (to_dom %list);
                      form_inputs_set_required (find_form_node a_node);
                      Lwt.return ()));
            }};
            [ list; add_a ]

      type ('arg, 'res) opt_component_configs_fun =
          ?elt:Form.config -> 'arg -> 'res
      let opt_component_configs_fun k ?elt = k elt

      let display ~value =
        Config.local_fun
          (fun local () ->
            let local = { local with Config.default = Some value } in
            opt_component_configs_fun
              (fun deep param_names ->
                pre_render true None
                  `Display
                  { Config.local ; deep }))
      let content ?submit =
        Config.local_fun
          (fun local () ->
            opt_component_configs_fun
              (fun deep () ->
                fun param_names ->
                  pre_render true submit
                    (`Param_names ("", param_names))
                    { Config.local ; deep }))
      let config =
        Config.local_fun
          (fun local () ->
            opt_component_configs_fun
              (fun deep () ->
                { Config.local ; deep }))

      let to_repr = List.map Form.to_repr
      let of_repr = List.map Form.of_repr
      let get_handler f = fun get post -> f (of_repr get) post
      let post_handler f = fun get post -> f get (of_repr post)

      let default_template : (a,param_names,template_data) Template.t =
        default_template

    end
end
}}

(*
module Form_option' = struct

  type ('a, 'template_data) option_template_data =
    < template : ('a, unit, 'template_data) Template.t ;
      template_data : 'template_data ;
      label : form_content option ;
      annotation : form_content option >

  type 'param_names option_param_names =
    [`One of bool] Eliom_parameter.param_name * 'param_names

  let component : 'a 'param_names 'deep_config 'template_data .
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
    fun ?label ?annotation ?default ?classes ?template ?template_data arg ->
      Config.local_fun
        (fun local deep ->
          { Config.local ; deep = Some deep })
        ?label ?annotation ?default ?classes ?template ?template_data arg

  let option_template : 'a 'param_names 'template_data .
      'template_data ->
      ( 'a,
        'param_names,
        'template_data
      ) Template.t ->
      ( 'a option,
        'param_names option_param_names,
        ( 'a, 'template_data ) option_template_data
      ) Template.t
    =
    fun the_template_data the_template ->
      Template.template
        (fun ~is_outmost ?submit ?label ?annotation ?default ?(a=[])
          ~(template_data : (_, _) option_template_data)
          (* ?(template_data=template_data ()) *) ~param_names field_renderings ->
            let checkbox_param_name, param_names, local_param_names =
          match param_names with
          | `Param_names (checkbox_param_name, param_names) ->
            `Param_names checkbox_param_name, `Param_names param_names, `Param_names ()
          | `Display -> `Display, `Display, `Display
        in
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
          let selector =
            let open Eliom_content.Html5.F in
            match checkbox_param_name with
            | `Display -> None
            | `Param_names checkbox_param_name ->
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
              Some [checkbox]
          in
          let label = Some (option_get ~default:[] template_data#label) in
          let content =
            let default = option_get ~default:None default in
            the_template
              (Template.arguments
                 ~is_outmost:false ?submit ?default ~param_names
                 ~template_data:the_template_data field_renderings)
          in
          Component_rendering.mk ?label ?selector ?annotation ~content () in
        let a = Eliom_content.Html5.F.a_class [form_option_class] :: a in
        let default = option_get ~default:None default in
        template_data#template
          (Template.arguments ~is_outmost ?submit ?label
             ?annotation:template_data#annotation ?default ~a
             ~template_data:template_data#template_data
             ~param_names:local_param_names [field_rendering]))

  module Make :
    functor (Options : Record_options) ->
      Form with
        type a = Options.a option and
        type repr = bool * Options.repr option and
        type param_names = Options.param_names option_param_names and
        type template_data = (Options.a, Options.template_data) option_template_data and
        type deep_config =
          ( Options.a, Options.param_names, Options.deep_config, Options.template_data
          ) Config.t option and
        type ('arg, 'res) opt_component_configs_fun =
          ( Options.a, Options.param_names, Options.deep_config, Options.template_data
          ) Config.t option ->
          'arg -> 'res
  =
    functor (Options : Record_options) -> struct
      module Options = struct
        type a = Options.a option
        type repr = bool * Options.repr option
        type param_names = Options.param_names option_param_names
        type template_data = (Options.a, Options.template_data) option_template_data
        type 'res template_data_fun = ?template:(Options.a, unit, Options.template_data) Template.t -> ?template_data:Options.template_data -> ?label:form_content -> ?annotation:form_content -> unit -> 'res
        let apply_template_data_fun (f : _ template_data_fun) = f ()
        let pre_template_data  =
          fun k ?(template=default_template)
            ?(template_data=Options.apply_template_data_fun (Options.pre_template_data identity))
            ?label ?annotation () ->
          k (object
               method template = template
               method template_data = template_data
               method label = label
               method annotation = annotation
             end)

        let params_type prefix =
          prefix,
          Eliom_parameter.prod
            (Eliom_parameter.bool (prefix^"|is_some"))
            (Eliom_parameter.opt (Options.params_type (prefix^"|some")))
        type deep_config =
          ( Options.a, Options.param_names, Options.deep_config, Options.template_data
          ) Config.t option
        let default_deep_config = None

        let to_repr = function
          | None -> false, None
          | Some x -> true, Some (Options.to_repr x)
        let of_repr = function
          | (false, _) -> None
          | (true, Some x) -> Some (Options.of_repr x)
          | _ -> failwith "Form_option_functor.of_repr"

        type ('arg, 'res) opt_component_configs_fun =
            deep_config -> 'arg -> 'res

        let opt_component_configs_fun f deep_config arg = f deep_config arg

        let component_names = [ "" ]

        let suffix_Some = "|Some"

        let fields =
          let module Field = struct
            type enclosing_a = a
            type enclosing_param_names = param_names
            type enclosing_deep_config = deep_config
            let project_default x = x
            let project_param_names (_, param_names) =
              param_names
            let project_config deep_config = deep_config
            include Make_record (Options)
          end in
          [ ((module Field) : (a, param_names, deep_config) field) ]

        let default_template =
          option_template
            Options.(apply_template_data_fun (pre_template_data identity))
            default_template
      end
      include Make_record (Options)
    end
end

module type Bijection = sig
  include Repr
  val of_string : string -> repr
  val to_string : repr -> string
  type template_data
  type original_template_data
  val import_template_data : original_template_data -> template_data
  val export_template_data : template_data -> original_template_data
end
*)
(* module Bijection_options = *)
(*   functor (Options : Options) -> *)
(*     functor (Bijection : Bijection *)
(*              with type repr = Options.a *)
(*              and type original_template_data = Options.template_data) -> *)
(*       (struct *)
(*         include Bijection *)
(*         type param_names = [`One of repr] Eliom_parameter.param_name *)
(*         type deep_config = Options.deep_config *)
(*         let default_deep_config = Options.default_deep_config *)
(*         let params_type prefix = *)
(*           Eliom_parameter.user_type *)
(*             ~to_string:Bijection.to_string *)
(*             ~of_string:Bijection.of_string *)
(*             prefix *)
(*         let default_template_data ?default () = *)
(*           option_map *)
(*             Bijection.import_template_data *)
(*             (Options.default_template_data *)
(*                ?default:(option_map Bijection.to_repr default) ()) *)
(*         let default_template ~is_outmost ?submit ?label ?annotation ?default *)
(*             ?classes ?template_data ~param_names field_renderings = *)
(*           Options.default_template *)
(*             ~is_outmost ?submit ?label ?annotation *)
(*             ?default:(option_map Bijection.to_repr default) *)
(*             ?classes *)
(*             ?template_data:(option_map Bijection.export_template_data template_data) *)
(*             ~param_names field_renderings *)
(*         let fields = *)
(*           List.map *)
(*             (fun (field : (Options.a, Options.param_names, Options.deep_config) field) -> *)
(*               let module Field = (val field) in *)
(*               let module Field = struct *)
(*                 include (Field : Pre_form) *)
(*                 type enclosing_a = Bijection.a *)
(*                 let project_default enclosing = *)
(*                   Field.project_default (Bijection.to_repr enclosing) *)
(*                 type enclosing_param_names = [`One of repr] Eliom_parameter.param_name *)
(*                 let project_param_names = Field.project_param_names *)
(*               end in *)
(*               ((module Field) : (a, param_names, deep_config) field)) *)
(*           Options.fields *)
(*         let field_names = Options.field_names *)
(*         type ('arg, 'res) opt_component_configs_fun = ('arg, 'res)  Options.opt_component_configs_fun *)
(*         let opt_component_configs_fun = Options.opt_component_configs_fun *)
(*        end : Options with type a = Bijection.a *)
(*                      and type template_data = Bijection.template_data *)
(*                      and type param_names = [`One of Bijection.repr] Eliom_parameter.param_name *)
(*                      and type deep_config = Options.deep_config) *)

module Types = struct
  type string_or_empty = string
  module Form_string_or_empty = struct
    module Atomic_options = struct
      include Form_string.Atomic_options
      let default_template = form_string_default_template true
    end
    module Options = Make_atomic_options (Atomic_options)
    include Make_record (Options)
  end
end
