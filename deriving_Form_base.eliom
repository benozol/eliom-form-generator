(* TODO

   * Make constant values unbreakable
        - having all fields of type a as (string, a) either
        - with string is the encrypted json representation of the constant value

   * Rename param_names to params

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

type 'a default_constant_value = [`Default of 'a | `Constant of 'a]
type default_constant' = [`Default|`Constant]
let default_constant_map ~f = function
  | `Constant a -> `Constant (f a)
  | `Default a -> `Default (f a)
let default_constant_get = function
  | `Default x | `Constant x -> x
let default_constant_put_over_option :
    'a option default_constant_value -> 'a default_constant_value option =
  fun dc ->
    match default_constant_get dc with
    | Some a -> Some (default_constant_map ~f:(constant a) dc)
    | None -> None
let default_constant = function
  | `Constant _ -> `Constant
  | `Default _ -> `Default
let hidden_value value =
  option_map ~f:default_constant value = Some `Constant,
  option_map ~f:default_constant_get value

module Component_rendering = struct
  type t = {
    label : form_content option;
    selector : form_content option;
    content : form_content;
    annotation : form_content option;
    a : Html5_types.div_attrib Eliom_content.Html5.F.attrib list option;
    default_constant : default_constant' option;
  }
  let bind { label ; selector ; annotation ; a ; content ; default_constant } f =
    f ?label ?selector ~content ?annotation ?a ?default_constant ()
end

module Pre_local_config = struct
  type 'a t = {
    label : form_content option;
    annotation : form_content option;
    value :  'a default_constant_value option;
    a : Html5_types.div_attrib Eliom_content.Html5.F.attrib list option;
  }
  let zero = {
    label = None; annotation = None; value = None; a = None
  }
  let bind x f =
    let { label ; annotation ; value ; a } = x in
    f ?label ?annotation ?value ?a ()
  let option_or_by_field cs =
    List.fold_left
      (fun c1 c2 -> {
        label = option_or [c1.label; c2.label];
        annotation = option_or [c1.annotation; c2.annotation];
        value = option_or [c1.value; c2.value];
        a = option_or [c1.a; c2.a];
       })
      zero cs
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
    ('a, 'param_names, 'template_data) arguments -> form_content

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
    value :  'a default_constant_value option;
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
    ?value:'a default_constant_value ->
    ?a : Html5_types.div_attrib Eliom_content.Html5.F.attrib list ->
    ?template:('a, 'param_names, 'template_data) Template.t ->
    ?template_data:'template_data ->
    'arg -> 'res

  let zero = { pre = Pre_local_config.zero ; template = None ; template_data = None }

  let fun_ : _ -> (_, _, _, unit, _) fun_ =
    fun k ?label ?annotation ?value ?a ?template ?template_data arg ->
      let pre = Pre_local_config.({ label ; annotation ; value ; a }) in
      k { pre ; template; template_data } arg

  let bind { pre = { label ; annotation ; value ; a } ; template ; template_data } k =
    k ?label ?annotation ?value ?a ?template ?template_data ()

  let option_or_by_field cs =
    List.fold_left
      (fun c1 c2 -> {
        pre = Pre_local_config.option_or_by_field [c1.pre; c2.pre];
        template = option_or [c1.template; c2.template];
        template_data = option_or [c1.template_data; c2.template_data];
      })
      zero cs

end

type ('a, 'param_names, 'template_data, 'deep_config) config' = {
  local : ('a, 'param_names, 'template_data) Local_config.t;
  deep : 'deep_config;
}

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
                          (fun ?(label=[]) ?selector ~content ?annotation ?(a=[]) ?default_constant () ->
                            if selector = None && annotation = None && content = [] then
                              None
                            else
                              let label =
                                td ~a:[a_class ["label"]] label
                              in
                              let selector =
                                option_to_list
                                  (option_map
                                     ~f:(td ~a:[a_class ["selector"]])
                                     selector)
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
                                if default_constant = Some `Constant then
                                  [ a_style "display: none" ]
                                else []
                              in
                              Some (tr ~a:(a_class ["field"] :: maybe_hidden @ a)
                                      (selector @ label :: content :: annotation))))
                   field_renderings
               in
               let contents = captions @ fields @ annotations @ submits in
               let outmost_class = if is_outmost then ["outmost"] else [] in
               match contents with
               | [] -> []
               | hd :: tl ->
                 [ table ~a:(a_class (["form"; form_class] @ outmost_class) :: a)
                     hd tl ]))
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

(******************************************************************************)

module type Repr = sig
  type t
  type repr
  val of_repr : repr -> t
  val to_repr : t -> repr
end

module type Template_data = sig
  type a
  type template_data
  type 'res template_data_fun
  val pre_template_data : value:a option -> (template_data -> 'res) -> 'res template_data_fun
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
  type param_names
  type deep_config
  type ('arg, 'res) opt_component_configs_fun
  include Repr with type t := a
  include Template_data with type a := a
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
    (a, param_names, template_data, deep_config) config' -> form_content
end

module type Form = sig
  include Pre_form
  type config = (a, param_names, template_data, deep_config) config'
  val params_type : string -> (repr, [`WithoutSuffix], param_names) Eliom_parameter.params_type
  val template_data : value:a option -> template_data template_data_fun
  val content :
    ?submit:button_content ->
    ( a, param_names, template_data,
      unit,
      (unit,
       param_names -> form_content) opt_component_configs_fun
    ) Local_config.fun_
  val display :
    value:a ->
    ( a, param_names, template_data,
      unit,
      (unit,
       form_content) opt_component_configs_fun ) Local_config.fun_
  val config :
    ( a, param_names, template_data,
      unit,
      (unit,
       config) opt_component_configs_fun ) Local_config.fun_
  val get_handler : (a -> 'post -> 'res) -> (repr -> 'post -> 'res)
  val post_handler : ('get -> a -> 'res) -> ('get -> repr -> 'res)
end

(******************************************************************************)

module type Field = sig
  include Pre_form
  type enclosing_a
  type enclosing_param_names
  type enclosing_deep_config
  val project_value : enclosing_a -> a option
  val project_param_names : enclosing_param_names -> param_names
  val project_config : enclosing_deep_config ->
    (a, param_names, template_data, deep_config) config' option
  val prefix : string -> string
end

module type Variant = sig
  include Field
  val is_constructor : enclosing_a -> bool
end

(******************************************************************************)

module Make_base (Options : Base_options) = struct
  include Options
  let params_type prefix = snd (params_type' (prefix^param_name_root))
  let template_data ~value = pre_template_data ~value identity
  type config = (a, param_names, template_data, deep_config) config'
  let config :
      ( a, param_names, template_data, unit, (unit, config) opt_component_configs_fun
      ) Local_config.fun_ =
    Local_config.fun_
      (fun local () ->
        Options.opt_component_configs_fun
          (fun deep () ->
            { local ; deep }))
  let pre_content pre_render ?submit =
    Local_config.fun_
      (fun local () ->
        Options.opt_component_configs_fun
          (fun deep () ->
            fun param_names ->
              pre_render true submit
                (`Param_names ("", param_names))
                { local ; deep }))
  let pre_display pre_render ~value =
    Local_config.fun_
      (fun local () ->
        let local =
          Local_config.({
            local with
              pre = {
                local.pre with
                  value = Some (`Default value)
              }
          })
        in
        Options.opt_component_configs_fun
          (fun deep () ->
            pre_render true None
              `Display
              { local ; deep }))
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
    match contains form_record_class, contains form_sum_class with
      | true, false -> `Record
      | false, true ->
        if contains form_sum_dropdown_class then
          `Sum `Drop_down
        else failwith "classify_form_node: sum"
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
