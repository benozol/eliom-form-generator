
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
let form_list_list_class = "__eliom_form_list_list__"
let form_list_list_item_class = "__eliom_form_list_list_item__"
let form_list_remove_button_class = "__eliom_form_list_remove_button__"

let prefix_concat ~prefix suffix = prefix ^ "|" ^ suffix
let param_name_root = "__eliom_form__"

let form_sum_variant_attribute =
  Eliom_content.Html5.Custom_data.create
    ~name:"__eliom_form_sum_variant__"
    ~default:""
    ~to_string:identity
    ~of_string:identity
    ()

let input_marker =
  Eliom_content.Html5.F.(span ~a:[a_class [input_marker_class]] [])

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

  let parent_with_class =
    fun ?(strict=true) class_ element ->
      if not strict && Js.to_bool (element ## classList ## contains (Js.string class_)) then
        Some element
      else
        let rec aux element =
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
                aux parent)
        in
        aux element

  let find_form_node ?strict node =
    match parent_with_class ?strict form_class (node :> Dom_html.element Js.t) with
      | Some form_node -> form_node
      | None -> raise Not_found

  let classify_form_node form_node =
    let form_node = (form_node :> Dom_html.element Js.t) in
    let contains clazz = Js.to_bool (form_node ## classList ## contains (Js.string clazz)) in
    if not (contains form_class) then
      failwith "classify_form_node";
    match contains form_record_class, contains form_sum_class, contains form_option_class with
      | true, false, false -> `Record
      | false, true, false ->
        begin match contains form_sum_radio_class, contains form_sum_dropdown_class with
          | false, true -> `Sum `Drop_down
          | _ -> failwith "classify_form_node: sum"
        end
      | _ -> failwith "classify_form_node"

  let rec nodes_between ~root ~descendent =
    Js.Opt.case (descendent ## parentNode)
      (fun () -> failwith "nodes_between")
      (fun parent ->
        if parent == root then
          []
        else
          parent :: nodes_between ~root ~descendent:parent)

  let rec is_required_rec : Dom_html.element Js.t -> bool =
    fun node ->
      try
        let form_node = find_form_node node in
        let locally_required =
          match classify_form_node form_node with
          | `Record -> true
          | `Sum `Drop_down ->
              if Js.to_bool (node ## classList ## contains
                   (Js.string form_sum_dropdown_variant_selector_class)) then
                true
              else begin
                let selected_variant_name =
                  Eliom_content.Html5.Custom_data.get_dom form_node
                    form_sum_variant_attribute
                in
                let variant_node =
                  option_get' ~default:(fun () -> failwith "is_required_rec: variant")
                    (parent_with_class ~strict:false form_sum_variant_class node) in
                let variant_name =
                  Eliom_content.Html5.Custom_data.get_dom variant_node
                    form_sum_variant_attribute
                in
                variant_name = selected_variant_name
              end
        in
        locally_required && is_required_rec form_node
      with
        Not_found -> true
  let is_required_rec node = is_required_rec (node :> Dom_html.element Js.t)

  let form_inputs_set_required form_node =
    if not (Js.to_bool (form_node ## classList ## contains (Js.string form_class))) then
      failwith "form_inputs_set_required";
    let inputs =
      Dom.list_of_nodeList
        (form_node ## querySelectorAll
           (ksprintf Js.string
              "input:not([type='checkbox']):not(.%s),\
               select:not(.%s)"
              component_not_required_class
              component_not_required_class))
    in
    Firebug.console ## log_4 (Js.string "form_inputs_set_required on", form_node, List.length inputs, inputs);
    List.iter
      (fun node ->
        Js.Opt.iter (Dom_html.CoerceTo.input node)
          (fun input ->
            let is_required = is_required_rec input in
            Firebug.console ## log_3 (Js.string "input", is_required, input);
            input ## required <- Js.bool is_required);
        Js.Opt.iter (Dom_html.CoerceTo.select node)
          (fun select ->
            let is_required = is_required_rec select in
            Firebug.console ## log_3 (Js.string "select", is_required, select);
            select ## required <- Js.bool is_required);
        ())
      inputs;
    let variants =
      Dom.list_of_nodeList
        (form_node ## querySelectorAll
           (ksprintf Js.string ".%s" form_sum_variant_class))
    in
    List.iter
      (fun variant ->
        let is_required = is_required_rec variant in
        Firebug.console ## log_3 (Js.string "variant", is_required, variant);
        variant ## style ## display <- Js.string (if is_required then "" else "none"))
      variants;
    ()

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

let set_required_for_outmost ~is_outmost = function
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
}}
