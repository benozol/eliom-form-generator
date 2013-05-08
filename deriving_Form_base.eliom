(* TODO

   * Make constant values unbreakable
        - having all fields of type a as (string, a) either
        - with string is the encrypted json representation of the constant value
*)


(******************************************************************************)
{shared{

open Deriving_Form_utils

let form_class = "__eliom_form__"
let form_sum_class = "__eliom_form_sum__"
let form_sum_variant_class = "__eliom_form_sum_variant__"
let form_sum_dropdown_variant_selector_class = "__eliom_form_sum_variant_selector__"
let form_sum_radio_class = "__eliom_form_sum_radio__"
let form_sum_dropdown_class = "__eliom_form_sum_dropdown__"
let form_record_class = "__eliom_form_record__"
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

type 'param_names or_display = [ `Display | `Param_names of string * 'param_names ]

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

type 'a default_constant = [`Default of 'a | `Constant of 'a]
let default_constant_map ~f = function
  | `Constant a -> `Constant (f a)
  | `Default a -> `Default (f a)
let default_constant_get = function
  | `Default x | `Constant x -> x
let default_constant_put_over_option :
    'a option default_constant -> 'a default_constant option =
  fun dc ->
    match default_constant_get dc with
    | Some a -> Some (default_constant_map ~f:(constant a) dc)
    | None -> None
type default_or_constant = [` Default | `Constant ]
let default_or_constant = function
  | `Constant _ -> `Constant
  | `Default _ -> `Default
let hidden_value value =
  option_map ~f:default_or_constant value = Some `Constant,
  option_map ~f:default_constant_get value

module Component_rendering = struct
  type surrounding = {
    a : Html5_types.div_attrib Eliom_content.Html5.F.attrib list option;
    label : form_content option;
    annotation : form_content option;
    value : default_or_constant option;
  }
  type t = {
    content : form_content;
    surrounding : surrounding;
  }
  let surrounding_zero =
    { a = None ; label = None ; annotation = None ; value = None }
  let bind { content ; surrounding } k =
    let { a ; label ; annotation ; value } = surrounding in
    k ~content ?a ?label ?annotation ?value ()
end

module Pre_local_config = struct
  type 'a t = {
    label : form_content option;
    annotation : form_content option;
    value :  'a default_constant option;
    a : Html5_types.div_attrib Eliom_content.Html5.F.attrib list option;
  }
  let zero = {
    label = None; annotation = None; value = None; a = None
  }
  let bind x f =
    let { label ; annotation ; value ; a } = x in
    f ?label ?annotation ?value ?a ()
  let option_or_by_field c1 c2 = {
    label = option_or c1.label c2.label;
    annotation = option_or c1.annotation c2.annotation;
    value = option_or c1.value c2.value;
    a = option_or c1.a c2.a;
  }
  let to_surrounding { a ; label ; annotation ; value } =
    let value = option_map ~f:default_or_constant value in
    { Component_rendering.a ; label ; annotation ; value }
end

module Template = struct

  type ('a, 'param_names, 'template_data) arguments = {
    is_outmost : bool ;
    submit : button_content option ;
    config : 'a Pre_local_config.t;
    template_data : 'template_data ;
    param_names : 'param_names or_display ;
    component_renderings : Component_rendering.t list ;
  }

  type ('a, 'param_names, 'template_data) t =
    ('a, 'param_names, 'template_data) arguments -> form_content Lwt.t

  let arguments ~is_outmost ?submit ?(config=Pre_local_config.zero) ~template_data
      ~param_names ~component_renderings () =
    { is_outmost ; submit ; config ; template_data ;
      param_names ; component_renderings }

  let template f arguments =
    let { is_outmost ; submit ; config ; template_data ;
          param_names ; component_renderings }
        = arguments
    in
      f ~is_outmost ?submit ~config ~template_data
        ~param_names ~component_renderings ()
end

module Local_config = struct

  type 'a pre = 'a Pre_local_config.t = {
    label : form_content option;
    annotation : form_content option;
    value :  'a default_constant option;
    a : Html5_types.div_attrib Eliom_content.Html5.F.attrib list option;
  }

  type ('a, 'param_names, 'template_data) t = {
    pre : 'a Pre_local_config.t;
    template : ('a, 'param_names, 'template_data) Template.t option;
    template_data : 'template_data option;
  }

  type ('a, 'param_names, 'template_data, 'arg, 'res) fun_ =
    ?label:form_content ->
    ?annotation:form_content ->
    ?value:'a default_constant ->
    ?a : Html5_types.div_attrib Eliom_content.Html5.F.attrib list ->
    ?template:('a, 'param_names, 'template_data) Template.t ->
    ?template_data:'template_data ->
    'arg -> 'res

  let zero = { pre = Pre_local_config.zero ; template = None ; template_data = None }

  let fun_ : _ -> (_, _, _, _, _) fun_ =
    fun k ?label ?annotation ?value ?a ?template ?template_data arg ->
      let pre = Pre_local_config.({ label ; annotation ; value ; a }) in
      k { pre ; template; template_data } arg

  let bind { pre = { label ; annotation ; value ; a } ; template ; template_data } k arg =
    k ?label ?annotation ?value ?a ?template ?template_data arg

  let option_or_by_field c1 c2 = {
    pre = Pre_local_config.option_or_by_field c1.pre c2.pre;
    template = option_or c1.template c2.template;
    template_data = option_or c1.template_data c2.template_data;
  }

  let update : ('a, 'param_names, 'template_data, ('a, 'param_names, 'template_data) t, ('a, 'param_names, 'template_data) t) fun_ =
    fun ?label ->
      fun_
        (fun update config ->
          option_or_by_field update config)
        ?label

  let for_component ?value ?component_name ~local ~local_override =
    let local_from_value =
      let value = option_bind ~f:default_constant_put_over_option value in
      update ?value zero in
    let local_from_fieldname =
      let label = option_map ~f:(list_singleton -| pcdata -| default_label_of_component_name) component_name in
      update ?label zero in
    List.fold_left option_or_by_field zero
      [ local_override; local; local_from_value; local_from_fieldname ]

end

type ('a, 'param_names, 'template_data, 'deep_config) config' = {
  local : ('a, 'param_names, 'template_data) Local_config.t;
  deep : 'deep_config;
}

let project_config_override_config field_name project_value opt_value project_config default_config deep_config deep_config_override =
  let config_override = option_get ~default:default_config (project_config deep_config_override) in
  let config =
    let value = option_map ~f:(default_constant_map ~f:project_value) opt_value in
    let config = option_get ~default:default_config (project_config deep_config) in
    let local =
      Local_config.for_component ?value ~component_name:field_name
        ~local:config.local ~local_override:config_override.local
    in
    { config with local }
  in
  config, config_override


let template_table =
  fun arguments ->
    Template.template
      (let open Eliom_content.Html5.F in
       fun ~is_outmost ?submit ~config ~template_data:_ ~param_names:_
         ~component_renderings:field_renderings () ->
           Pre_local_config.bind config
             (fun ?label ?annotation ?value:_ ?(a=[]) () ->
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
                       (flip Component_rendering.bind
                          (fun ~content ?(a=[]) ?(label=[]) ?annotation ?value () ->
                            if annotation = None && content = [] then
                              None
                            else
                              let label =
                                td ~a:[a_class ["label"]] label
                              in
                              let content =
                                td ~a:[a_class ["content"]] content
                              in
                              let annotation =
                                option_to_list
                                  (option_map
                                     ~f:(td ~a:[a_class ["annotation"]])
                                     annotation)
                              in
                              let maybe_hidden =
                                if value = Some `Constant then
                                  [ a_style "display: none" ]
                                else []
                              in
                              Some (tr ~a:(a_class ["field"] :: maybe_hidden @ a)
                                      (label :: content :: annotation))))
                   field_renderings
               in
               let contents = captions @ fields @ annotations @ submits in
               let outmost_class = if is_outmost then ["outmost"] else [] in
               match contents with
               | [] -> Lwt.return []
               | hd :: tl ->
                 Lwt.return [
                   table ~a:(a_class (["form"; form_class] @ outmost_class) :: a)
                     hd tl
                 ]))
      arguments

let default_template =
  template_table


type id_repr = int option
type id_param_name = [`One of int] Eliom_parameter.param_name
let id_param_name = Eliom_parameter.(opt (int "__eliom_form_id__"))
let id_input ?id ~name : form_content =
  match id with
  | None -> []
  | Some id ->
    Eliom_content.Html5.F.([
      int_input ~value:id ~name ~input_type:`Hidden ()
    ])

(******************************************************************************)

module type Raw_repr = sig
  type t
  type raw_repr
  val of_raw_repr : raw_repr -> t
  val to_raw_repr : t -> raw_repr
end

module type Template_data = sig
  type a
  type template_data
  type 'res template_data_fun
  val pre_template_data : value:a option -> (template_data -> 'res Lwt.t) -> 'res Lwt.t template_data_fun
  val apply_template_data_fun : 'res template_data_fun -> 'res
end

module Template_data_unit :
  functor (T : sig type t end) ->
    Template_data with
      type template_data = unit and
      type 'res template_data_fun = 'res and
      type a := T.t =
  functor (T : sig type t end) -> struct
    type a = T.t
    type template_data = unit
    type 'res template_data_fun = 'res
    let pre_template_data ~value:_ k = k ()
    let apply_template_data_fun (f : _ template_data_fun) = f
  end

module type Base_options = sig
  type a
  type raw_param_names
  type deep_config
  type ('arg, 'res) opt_component_configs_fun
  include Raw_repr with type t := a
  include Template_data with type a := a
  val params' : string -> string * (raw_repr, [`WithoutSuffix], raw_param_names) Eliom_parameter.params_type
  val opt_component_configs_fun : (deep_config -> 'arg -> 'res) ->
    ('arg, 'res) opt_component_configs_fun
  val default_deep_config : deep_config
  val default_template : (a, raw_param_names, template_data) Template.t
  val component_names : string list
end

module type Pre_form = sig
  include Base_options
  type config = (a, raw_param_names, template_data, deep_config) config'
  val pre_render : bool -> button_content option -> raw_param_names or_display ->
    config:config -> config_override:config -> form_content Lwt.t
end

module type Form = sig
  include Pre_form
  type repr = raw_repr * id_repr
  type param_names = raw_param_names * id_param_name
  type id
  val repr : a -> repr
  val fresh_id : unit -> id
  val set_config_once : ?id:id -> config -> unit
  val params : string -> (repr, [`WithoutSuffix], param_names) Eliom_parameter.params_type
  val template_data : value:a option -> template_data Lwt.t template_data_fun
  val content :
    ?submit:button_content -> ?id:id ->
    ( a, raw_param_names, template_data,
      unit,
      (unit,
       param_names -> form_content Lwt.t) opt_component_configs_fun
    ) Local_config.fun_
  val display :
    value:a ->
    ( a, raw_param_names, template_data,
      unit,
      (unit,
       form_content Lwt.t) opt_component_configs_fun ) Local_config.fun_
  val config :
    ( a, raw_param_names, template_data,
      unit,
      (unit,
       config) opt_component_configs_fun ) Local_config.fun_
  val get_handler : (?id:id -> a -> 'post -> 'res) -> (repr -> 'post -> 'res)
  val post_handler : (?id:id -> 'get -> a -> 'res) -> ('get -> repr -> 'res)
end

(******************************************************************************)

module type Field = sig
  include Pre_form
  type enclosing_a
  type enclosing_raw_param_names
  type enclosing_deep_config
  val project_value : enclosing_a -> a option
  val project_param_names : enclosing_raw_param_names -> raw_param_names
  val project_config : enclosing_deep_config ->
    (a, raw_param_names, template_data, deep_config) config' option
  val prefix : string -> string
end

module type Variant = sig
  include Field
  val is_constructor : enclosing_a -> bool
end

(******************************************************************************)

module Make_id (Config : sig type t end) = struct
  type id = int
  let fresh_id =
    let counter = ref 0 in
    fun () ->
      incr counter;
      !counter
  let configs : (id option, Config.t) Hashtbl.t = Hashtbl.create 13
  let set_config_once ?id config =
    Hashtbl.add configs id config
  let get_config_once id =
    try
      let config = Hashtbl.find configs id in
      Hashtbl.remove configs id;
      Some config
    with Not_found ->
      None
end

module Make_base (Options : Base_options) = struct
  include Options
  type repr = raw_repr * id_repr
  type param_names = raw_param_names * id_param_name
  let params prefix : (repr, [`WithoutSuffix], param_names) Eliom_parameter.params_type =
    Eliom_parameter.prod
      (snd (params' (prefix^param_name_root)))
      id_param_name
  let template_data ~value = pre_template_data ~value Lwt.return
  type config = (a, raw_param_names, template_data, deep_config) config'
  include Make_id (struct type t = config end)
  let repr a = to_raw_repr a, None
  let config :
      ( a, raw_param_names, template_data, unit, (unit, config) opt_component_configs_fun
       ) Local_config.fun_  =
    Local_config.fun_
      (fun local () ->
        Options.opt_component_configs_fun
          (fun deep () ->
            { local ; deep }))

  let config_zero = {
    local = Local_config.zero ;
    deep = Options.default_deep_config ;
  }

  let pre_content pre_render ?submit ?id =
    Local_config.fun_
      (fun local () ->
        Options.opt_component_configs_fun
          (fun deep () ->
            fun (param_names, id_param_name) ->
              let param_names = `Param_names ("", param_names) in
              let config = { local ; deep } in
              let config_override = option_get ~default:config_zero (get_config_once id) in
              Lwt.map
                (List.append (id_input ?id ~name:id_param_name))
                (pre_render true submit param_names ~config ~config_override)))

  let pre_display pre_render ~value =
    Local_config.fun_
      (fun local () ->
        let local =
          let value = Some (`Default value) in
          Local_config.update ?value local
        in
        Options.opt_component_configs_fun
          (fun deep () ->
            let config = { local ; deep } in
            pre_render true None `Display ~config ~config_override:config_zero))

  let get_handler f =
    fun (repr, id) post ->
      f ?id (of_raw_repr repr) post

  let post_handler f =
    fun get (repr, id) ->
      f ?id get (of_raw_repr repr)
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
    match contains form_record_class, contains form_sum_class with
      | true, false -> `Record
      | false, true ->
        `Sum `Drop_down
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
