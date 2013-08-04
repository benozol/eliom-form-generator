

{shared{

  let css_name = "eliom-deriving-form.css"

  open Html5_types
  open Eliom_content
  open Printf
  type 'a elt = 'a Eliom_content.Html5.elt

  external (@) : ('a -> 'b) -> 'a -> 'b = "%apply"
  external (<@) : ('a -> 'b) -> 'a -> 'b = "%apply"
  external identity : 'a -> 'a = "%identity"
  let (-|) f g x = f (g x)
  let (|-) f g x = g (f x)
  let (@@) = List.append
  let flip f x y = f y x
  let flip2 f x y z = f z x y
  let if_applies p x =
    if p x then [ x ] else []
  let before f x = f x; x
  module Option = struct
    let map f = function
      | None -> None
      | Some x -> Some (f x)
    let get default = function
      | None -> default
      | Some x -> x
    let get' f = function
      | None -> f ()
      | Some x -> x
    let bind opt f =
      match opt with
      | Some x -> f x
      | None -> None
    let from_list = function
      | [] -> None
      | x :: _ -> Some x
    let iter f = function
      | None -> ()
      | Some x -> f x
  end
}}
{client{
  let trace = Eliom_lib.trace
  let trace_any any fmt =
    let k str =
      if false then
        Firebug.console ## log_2 (Js.string str, any)
      else
        ()
    in
    Printf.ksprintf k fmt

  let ($) node selector = node ## querySelector (Js.string selector)
  let ($$) node selector = node ## querySelectorAll (Js.string selector)
}}
{server{
  let trace fmt = ksprintf (fun str -> Ocsigen_messages.console @ fun () -> str) fmt
}}

{shared{

  let form_outmost_class = "eliom-form-outmost"
  let form_class = "eliom-form"
  let atomic_class = "eliom-form-atomic"
  let option_class = "eliom-form-option"
  let option_selector_class = "eliom-form-option-selector"
  let option_content_class = "eliom-form-option-content"
  let sum_class = "eliom-form-sum"
  let sum_selector_class = "eliom-form-sum-selector"
  let sum_content_class = "eliom-form-sum-content"
  let sum_case_class = "eliom-form-sum-case"
  let sum_case_marker_class case = sum_case_class ^ "-" ^ case
  let tuple_class = "eliom-form-tuple"
  let record_class = "eliom-form-record"
  let record_field_marker_class field = "eliom-form-record-field-"^field
  let label_class = "eliom-form-label"
  let marker_class = "eliom-form-input-marker"
  let selector_snippet = "selector"
  let content_snippet = "content"
  let list_class = "eliom-form-list"
  let list_item_class = "eliom-form-list-item"
  let button_add_class = "eliom-form-add-elt"
  let button_remove_class = "eliom-form-remove-elt"
  let label_class = "eliom-form-label"
  let marker = Eliom_content.Html5.F.(span ~a:[a_class [marker_class]] [])
  let marked ?a input = Eliom_content.Html5.D.span ?a [ input ; marker ]
}}

{client{

  type control_element =
    [ | `Input of Dom_html.inputElement Js.t
      | `Select of Dom_html.selectElement Js.t
      | `Textarea of Dom_html.textAreaElement Js.t
      | `Fieldset of Dom_html.fieldSetElement Js.t ]

  let control_element node =
    let open Dom_html in
    match tagged node with
      | Input input -> Some (`Input input)
      | Select select -> Some (`Select select)
      | Textarea textarea -> Some (`Textarea textarea)
      | Fieldset fieldset -> Some (`Fieldset fieldset)
      | A _|Area _|Base _|Blockquote _|Body _|Br _|Button _|Canvas _|Caption _|
        Col _|Colgroup _|Del _|Div _|Dl _|Form _|Frameset _|Frame _|
        H1 _|H2 _|H3 _|H4 _|H5 _|H6 _|Head _|Hr _|Html _|Iframe _|Img _|Ins _|
        Label _|Legend _|Li _|Link _|Map _|Meta _|Object _|Ol _|Optgroup _|Option _|
        P _|Param _|Pre _|Q _|Script _|Style _|Table _|Tbody _|Td _|
        Tfoot _|Th _|Thead _|Title _|Tr _|Ul _|Other _ -> None

  let onload_or_now =
    let is_loaded = ref false in
    let () = Eliom_client.onload @ fun () -> is_loaded := true in
    fun f ->
      if !is_loaded then
        f ()
      else
        Lwt.async @ fun () ->
          lwt () = Eliom_client.wait_load_end () in
          Lwt.return @ f ()

  let js_string fmt = ksprintf Js.string fmt

  let with_class_id =
    let counter = ref 0 in
    fun node f ->
      let class_id = "__class_id__" ^ string_of_int (incr counter; !counter) in
      node ## classList ## add (Js.string class_id) ;
      let res = f class_id in
      node ## classList ## remove (Js.string class_id) ;
      res

  let set_required node is_required =
    let open Dom_html in
    match
      Option.get' (fun () -> Eliom_lib.error_any node "is_required") @
        control_element node
    with
      | `Input input -> input ## required <- Js.bool is_required
      | `Select select -> select ## required <- Js.bool is_required
      | `Textarea textarea -> textarea ## required <- Js.bool is_required
      | `Fieldset _ -> ()

  let has_class node class_ =
    let node =
      Js.Opt.get (Dom_html.CoerceTo.element (node :> Dom.node Js.t)) @
        fun () -> failwith "has_class"
    in
    Js.to_bool @ node ## classList ## contains (Js.string class_)

  let rec parent ?(inclusive=false) ?(before=fun _ -> false) predicate node =
    if before (node :> Dom.node Js.t) then
      None
    else
      if inclusive && predicate node then
        Some node
      else
        Js.Opt.case (node ## parentNode) (fun () -> None) @
          fun parent_node ->
            Js.Opt.case (Dom_html.CoerceTo.element parent_node)
              (fun () -> None)
              (parent ~inclusive:true ~before predicate)

  let parent ?inclusive ?before predicate node =
    parent ?inclusive ?before predicate (node :> Dom_html.element Js.t)

  let parent_with_class ?inclusive ?before class_ node =
    parent ?inclusive ?before (flip has_class class_) (node :> Dom_html.element Js.t)

  let rec path =
    fun ?(inclusive_from=false) ?(inclusive_to=false) from to_ ->
      if from = to_ then
        if inclusive_to then
          [ to_ ]
        else
          []
      else
        Js.Opt.case (from ## parentNode)
          (fun () -> Eliom_lib.error "Generate_form.path: to_ not in ancestors")
          (fun parent ->
            if inclusive_from then
              from :: path parent to_
            else
              path ~inclusive_from:true ~inclusive_to parent to_)
  let path ?inclusive_from ?inclusive_to from to_ =
    path ?inclusive_from ?inclusive_to
      (from :> Dom.node Js.t) (to_ :> Dom.node Js.t)

  let categorize_form node =
    if not @ has_class node form_class then
      failwith "Generate_form.categorize_form";
    let test (class_, _) = has_class node class_ in
    match
      List.filter test [
        option_class , `Option ;
        sum_class, `Sum ;
        tuple_class, `Tuple ;
        record_class, `Record  ;
        list_class, `List ;
        atomic_class, `Atomic
      ]
    with
      | [_, category] -> category
      | [] -> Eliom_lib.error_any node "No categories"
      | _ -> Eliom_lib.error_any node "Many categories"

  let querySelector ?(not_between=fun _ -> false) node fmt =
    flip ksprintf fmt @ fun selector ->
      trace_any node "querySelector: %s" selector;
      let node = (node :> Dom_html.element Js.t) in
      let nodes = Dom.list_of_nodeList (node $$ selector) in
      Option.from_list  @
        flip List.filter nodes @ fun node' ->
          not @ List.exists not_between @
            path ~inclusive_from:false ~inclusive_to:false
              node' node

  let querySelectorAll ?(not_between=fun _ -> false) node fmt =
    flip ksprintf fmt @ fun selector ->
      trace_any node "querySelectorAll: %s" selector;
      let node = (node :> Dom_html.element Js.t) in
      let nodes = Dom.list_of_nodeList (node $$ selector) in
      flip List.filter nodes @ fun node' ->
        not @ List.exists not_between @
          path ~inclusive_from:false ~inclusive_to:false
            node' node

  let option_selector form =
    trace_any form "option_selector";
    if not @ has_class form option_class then
      Eliom_lib.error_any form "option_selector: not an option";
    let input =
      Option.get' (fun () -> Eliom_lib.error_any form "Generate_form.option_selector: No selector") @
        querySelector ~not_between:(flip has_class form_class) form ".%s" option_selector_class
    in
    Js.Opt.get (Dom_html.CoerceTo.input input)
      (fun () -> Eliom_lib.error_any input "Generate_form.option_selector: Not input")

  let option_content form : #Dom_html.element Js.t =
    trace_any form "option_content";
    if not @ has_class form option_class then
      Eliom_lib.error_any form "option_content: not an option";
    Option.get' (fun () -> Eliom_lib.error_any form "Generate_form.option_selector: No content") @
      querySelector ~not_between:(flip has_class form_class) form ".%s" option_content_class

  let sum_selector form =
    trace_any form "sum_selector";
    if not @ has_class form sum_class then
      Eliom_lib.error_any form "sum_selector: not a sum";
    let select =
      Option.get' (fun () -> Eliom_lib.error_any form "Generate_form.sum_selector: No selector") @
        querySelector ~not_between:(flip has_class form_class) form ".%s" sum_selector_class
    in
    Js.Opt.get (Dom_html.CoerceTo.select select)
      (fun () -> Eliom_lib.error_any select "Generate_form.sum_selector: Not select")

  let sum_case form case =
    trace_any form "sum_case %s" case;
    if not @ has_class form sum_class then
      Eliom_lib.error_any form "sum_case: not a sum";
    Option.get' (fun () -> Eliom_lib.error_any form "Generate_form.sum_selector: No case %s" case) @
      querySelector ~not_between:(flip has_class form_class) form ".%s" (sum_case_marker_class case)

  let sum_cases form =
    trace_any form "sum_cases";
    if not @ has_class form sum_class then
      Eliom_lib.error_any form "sum_cases: not a sum";
    querySelectorAll ~not_between:(flip has_class form_class) form ".%s" sum_case_class

  let is_required_form_local form =
    trace_any form "is_required_local";
    if not @ has_class form form_class then
      Eliom_lib.error_any form "is_required_form_local";
    match parent_with_class form_class form with
      | None ->
        assert (has_class form form_outmost_class);
        true
      | Some parent ->
        match categorize_form parent with
          | `Tuple | `Record | `List | `Atomic -> true
          | `Option ->
            let selector = option_selector parent in
            Js.to_bool @ selector ## checked
          | `Sum ->
            let case = Js.to_string @ (sum_selector parent) ## value in
            if case = "" then
              false
            else
              let case_content = sum_case parent case in
              with_class_id form @ fun class_id ->
                Js.Opt.test (case_content $ sprintf ".%s" class_id)

  let rec is_required_form node =
    trace_any node "is_required_form";
    if not @ has_class node form_class then
      Eliom_lib.error_any node "is_required_form";
    is_required_form_local node &&
      match parent_with_class form_class node with
        | Some parent -> is_required_form parent
        | None -> true

  let set_sum_hidden form =
    trace_any form "set_sum_hidden";
    if not @ has_class form sum_class then
      Eliom_lib.error_any form "set_sum_hidden";
    let case = Js.to_string @ (sum_selector form) ## value in
    flip List.iter (sum_cases form) @ fun case_content ->
      if has_class case_content (sum_case_marker_class case) then
        case_content ## style ## display <- js_string ""
      else
        case_content ## style ## display <- js_string "none"

  let set_option_hidden form =
    trace_any form "set_option_hidden";
    if not @ has_class form option_class then
      Eliom_lib.error_any form "set_option_hidden";
    let option_content : Dom_html.element Js.t = option_content form in
    if Js.to_bool @ (option_selector form) ## checked then
      option_content ## style ## display <- js_string ""
    else
      option_content ## style ## display <- js_string "none"

  let is_required_atomic (node : Dom_html.element Js.t) =
    trace_any node "is_required_atomic";
    if not @ has_class node atomic_class then
      Eliom_lib.error_any node "is_required_atomic";
    (* let form = *)
    (*   Option.get' (fun () -> Eliom_lib.error_any node "is_required_atomic: not within generated form") @ *)
    (*     parent_with_class form_class node *)
    (* in *)
    is_required_form node

  let set_required_atomic (node : Dom_html.element Js.t) required =
    trace_any node "set_required_atomic";
    if not @ has_class node atomic_class then
      Eliom_lib.error_any node "set_required_atomic";
    let input =
      Js.Opt.get (node $ sprintf "input, textarea, select")
        (fun () -> Eliom_lib.error_any node "set_required_atomic: no control")
    in
    set_required input required

  let is_required_sum_selector node =
    trace_any node "is_required_sum_selector";
    if not @ has_class node sum_selector_class then
      Eliom_lib.error_any node "is_required_sum_selector";
    let form =
      Option.get' (fun () -> Eliom_lib.error_any node "is_required_sum_selector: not within generated form") @
        parent_with_class form_class node
    in
    is_required_form form

  let set_required_sum_selector node required =
    trace_any node "set_required_sum_selector";
    if not @ has_class node sum_selector_class then
      Eliom_lib.error_any node "set_required_sum_selector";
    set_required node required

  let reset_required (outmost : #Dom_html.element Js.t) =
    trace_any outmost "reset_required";
    if not @ has_class outmost form_outmost_class then
      Eliom_lib.error_any outmost "reset_required: not outmost";

    List.iter (fun input -> set_required_atomic input @ is_required_atomic input) @
      before (Eliom_lib.trace "%i atomics to reset required" -| List.length) @
        Dom.list_of_nodeList (outmost $$ sprintf ".%s" atomic_class);

    List.iter (fun input -> set_required_sum_selector input @ is_required_sum_selector input) @
      before (Eliom_lib.trace "%i sum selectors to reset required" -| List.length) @
        Dom.list_of_nodeList (outmost $$ sprintf ".%s" sum_selector_class);

    List.iter set_sum_hidden @
      before (Eliom_lib.trace "%i sums" -| List.length) @
        if_applies (flip has_class sum_class) outmost @@
          Dom.list_of_nodeList (outmost $$ sprintf ".%s" sum_class);

    List.iter set_option_hidden @
      before (Eliom_lib.trace "%i options" -| List.length) @
        if_applies (flip has_class option_class) outmost @@
          Dom.list_of_nodeList (outmost $$ sprintf ".%s" option_class);

    ()

  let rec data_from_form : 'a . #Dom.node Js.t -> 'a Deriving_Typerepr.t -> 'a =
    fun (type a) node (t : a Deriving_Typerepr.t) ->
      trace_any node "data_from_form";
      if not @ has_class node form_class then
        Eliom_lib.error_any node "Not a generated form";
      let node : Dom_html.element Js.t =
        Js.Opt.get (Dom_html.CoerceTo.element node) @
          fun () -> Eliom_lib.error_any node "data_from_form: not an html element"
      in
      let open Deriving_Typerepr in
      let aux_tuple : 'a . Dom_html.element Js.t -> 'a tuple -> 'a =
        fun (type a) node (t : a tuple) ->
          let category = categorize_form node in
          match (t : a tuple) with
            | Singleton t ->
              if category <> `Tuple then
                Eliom_lib.error_any node "data_from_form: not tuple (singleton)";
              let component =
                Js.Opt.get (node ## childNodes ## item (0)) @
                  fun () -> Eliom_lib.error_any node "data_from_form: no component for singleton tuple"
              in
              data_from_form component t
            | Composed components ->
              if category <> `Tuple then
                Eliom_lib.error_any node "data_from_form: not tuple (composed)";
              let component_contents =
                Dom.list_of_nodeList (node ## childNodes)
              in
              create_tuple components
                { create_tuple_component =
                    fun (Component (t, ix)) ->
                      data_from_form (List.nth component_contents ix) t }
      in
      let category = categorize_form node in
      let get_atomic_string name =
        if category <> `Atomic then
          Eliom_lib.error_any node "data_from_form: %s not atomic" name;
        Js.to_string @
          let control =
            Option.get' (fun () -> assert false) @
              control_element @
                Option.get' (fun () -> Eliom_lib.error_any node "data_from_form: atomic without control") @
                  querySelector node "input, textarea, select"
          in
          match control with
            | `Input input -> input ## value
            | `Textarea textarea -> textarea ## value
            | `Select select -> select ## value
            | `Fieldset _ -> assert false
      in
      match (t : a t) with
        | Unit ->
          if category <> `Atomic then
            Eliom_lib.error_any node "data_from_form: unit not atomic";
          (() : a)
        | String -> identity @ get_atomic_string "string"
        | Int -> int_of_string @ get_atomic_string "int"
        | Float -> float_of_string @ get_atomic_string "float"
        | Int32 -> Int32.of_string @ get_atomic_string "int32"
        | Int64 -> Int64.of_string @ get_atomic_string "int64"
        | Bool ->
          if category <> `Atomic then
            Eliom_lib.error_any node "data_from_form: bool not atomic";
          (match Option.bind (querySelector node "input[type=checkbox]") control_element with
            | Some (`Input input) -> Js.to_bool @ input ## checked
            | _ -> Eliom_lib.error_any node "data_from_form: no checkbox found in bool")
        | Tuple tuple -> aux_tuple node tuple
        | Option t ->
          if category <> `Option then
            Eliom_lib.error_any node "data_from_form: not option";
          let selector = option_selector node in
          let content =
            Js.Opt.get ((option_content node) ## childNodes ## item (0)) @
              fun () -> Eliom_lib.error_any node "data_from_form: option no content"
          in
          if Js.to_bool @ selector ## checked then
            Some (data_from_form content t)
          else
            None
        | List t ->
          if category <> `List then
            Eliom_lib.error_any node "data_from_form: not list";
          let ul =
            Js.Opt.get
              (Js.Opt.bind
                 (Js.Opt.bind
                    (node ## childNodes ## item (0))
                    Dom_html.CoerceTo.element)
                 Dom_html.CoerceTo.ul) @
              fun () -> Eliom_lib.error_any node "data_from_form: first element not an ul"
          in
          let items =
            flip List.map (Dom.list_of_nodeList @ ul ## childNodes) @ fun li ->
              let li =
                Js.Opt.get (Js.Opt.bind (Dom_html.CoerceTo.element li) Dom_html.CoerceTo.li) @
                  fun () -> Eliom_lib.error_any li "data_from_form: not a li"
              in
              if has_class li list_item_class then
                Some
                  (Js.Opt.get (li ## childNodes ## item (0)) @
                     fun () -> Eliom_lib.error_any li "data_from_form: no li content")
              else None
          in
          let items =
            List.map (function Some item -> item | None -> assert false) @
              List.filter ((<>) None) items
          in
          List.map (flip data_from_form t) items
        | Sum { summands } ->
          if category <> `Sum then
            Eliom_lib.error_any node "data_from_form: sum";
          let selector = sum_selector node in
          let case = Js.to_string @ selector ## value in
          if case = "" then
            Eliom_lib.error_any node "data_from_form: no case selected";
          let Any_summand summand = List.assoc case summands in
          (match summand with
            | Summand_constant _ as summand ->
              create_sum_case summand ()
            | Summand_alloc (_, tuple) as summand ->
              let content =
                Js.Opt.get
                  (Js.Opt.bind
                     ((sum_case node case) ## childNodes ## item (0))
                     Dom_html.CoerceTo.element) @
                  fun () -> Eliom_lib.error_any  node "data_from_form: no tuple for sum case %S" case
              in
              create_sum_case summand @ aux_tuple content tuple)
        | Record { fields } ->
          if category <> `Record then
            Eliom_lib.error_any node "data_from_form: not record";
          let field_contents =
            flip List.map fields @ fun (name, _) ->
              Option.get' (fun () -> Eliom_lib.error_any node "data_from_form: no field content for %S" name) @
                querySelector ~not_between:(flip has_class form_class)
                  node ".%s > *" (record_field_marker_class name)
          in
          let f =
            { create_record_field =
                let f : 'b . string -> (a, 'b) Deriving_Typerepr.field -> 'b =
                  fun _ field ->
                    let Field (ix, t) = field in
                    let content = List.nth field_contents ix in
                    data_from_form (content :> Dom.node Js.t) t
                in f }
          in
          create_record fields f
        | Function _ ->
          failwith "Generate_form.data_from_form"
        | Ref _ ->
          failwith "Generate_form.data_from_form"
        | Array _ ->
          failwith "Generate_form.data_from_form"

  let submit_form name any_t (form : Dom_html.formElement Js.t) =
    let open Deriving_Typerepr in
    trace_any form "submit_form";
    match any_t with
      | Any_t t ->
        let content =
          flip Option.get' (querySelector form ".%s" form_outmost_class) @
            fun () -> Eliom_lib.error_any form "submit_for: No outmost form"
        in
        let value = data_from_form (content :> Dom.node Js.t) t in
        Eliom_lib.debug "submit_form: %s (%s)"
          (show t value) (Eliom_lib.to_json value);
        let form' =
          Js.Opt.get
            (Js.Opt.bind
               (Dom_html.CoerceTo.element @ form ## cloneNode (Js._true))
               Dom_html.CoerceTo.form) @
            fun () -> assert false
        in
        begin
          let query = js_string "input[name=%S], textarea[name=%S], select[name=%S]" name name name in
          flip List.iter (Dom.list_of_nodeList @ form' ## querySelectorAll (query)) @ fun input ->
            Js.Opt.iter (input ## parentNode) @ fun parent ->
              ignore @ parent ## removeChild ((input :> Dom.node Js.t))
        end;
        let input = Html5.F.raw_input ~name ~input_type:`Text ~value:(Eliom_lib.to_json value) () in
        ignore @ form' ## appendChild (Html5.To_dom.of_node input);
        ignore @ form' ## onsubmit <- Eliom_client.form_handler;
        form' ## submit ()

  let aux_list_item_ref : (string -> Deriving_Typerepr.any_t -> Html5_types.li elt) ref =
    ref @ fun _ _ -> failwith "aux_list_item_ref"
}}

{shared{

  open Html5.F
  open Deriving_Typerepr

  type any_t_opt_value =
    | Any_t_opt_value : 'a t * 'a option -> any_t_opt_value

  let rec aux_form_tuple : 'a . ?a_local:_ -> ?value:'a -> string -> 'a tuple -> form_content elt =
    fun ?(a_local=[]) ?value name tuple ->
      Html5.D.div ~a:(a_class [ form_class ; tuple_class ] :: a_local) @
        match value with
          | None ->
            begin
              match tuple with
                | Singleton t ->
                  [ aux_form name t ]
                | Composed ts ->
                  aux_form_values name @
                    flip List.map ts @ fun (Any_component (Component (t, _))) ->
                      Any_t_opt_value (t, None)
            end
          | Some value ->
            match tuple with
              | Singleton t ->
                [ aux_form ~value name t ]
              | Composed components ->
                aux_form_values name @
                  let sub_values = get_tuple_components components value in
                  flip List.map sub_values @ fun (Dyn (t, value)) ->
                    Any_t_opt_value (t, Some value)

  and aux_form_values : string -> any_t_opt_value list -> form_content elt list =
    fun name t_values ->
      flip List.map t_values @ fun (Any_t_opt_value (t, value)) ->
        aux_form ?value name t

  and aux_form_sum  : 'a . ?a_local:_ -> ?value:'a -> string -> 'a sum -> form_content elt =
    fun ?(a_local=[]) ?value name { summands } ->
      let selector =
        let a = [ a_class [ sum_selector_class ] ] in
        let null = Html5.D.Option ([], "", Some (pcdata "- select -"), value = None) in
        let summands =
          flip List.map summands @ fun (summand_name, Any_summand summand) ->
            let sub_value = flip Option.map value @ get_sum_case summand in

            let selected = sub_value <> None in
            Html5.D.Option ([], summand_name, Some (pcdata summand_name), selected)
        in
        Html5.D.raw_select ~a ~name null summands
      in
      let content =
        div ~a:[a_class [sum_content_class]] @
          flip List.map summands @ fun (summand_name, Any_summand summand) ->
            div ~a:[a_class [sum_case_class; sum_case_marker_class summand_name]] @
              match summand with
                | Summand_constant _ -> []
                | Summand_alloc (_, tuple) ->
                  let value = Option.bind value (get_sum_case summand) in
                  [ aux_form_tuple ?value name tuple ]
      in
      ignore {unit{
        Lwt.async @ fun () ->
          (* lwt () = Eliom_client.wait_load_end () in *)
          let selector = Html5.To_dom.of_select %selector in
          Lwt_js_events.changes selector @ fun _ _ ->
            Lwt.return @
              match parent_with_class form_outmost_class selector with
                | Some outmost -> reset_required outmost
                | None -> failwith "Generate_form: no outmost"
      }};
      Html5.D.div ~a:(a_class [form_class; sum_class] :: a_local)
        [span [selector; marker]; content ]

  and aux_form_record : 'a . ?a_local:_ -> ?value:'a -> string -> 'a record -> form_content elt =
    fun ?(a_local=[]) ?value name ({ fields } as record) ->
      let values =
        match value with
          | None ->
            flip List.map fields @ fun (field_name, Any_field (Field (_, t))) ->
              field_name, Any_t_opt_value (t, None)
          | Some value ->
            flip List.map (get_record_fields record value) @ function
              | field_name, Dyn (t, sub_value) ->
                field_name, Any_t_opt_value (t, Some sub_value)
      in
      let rows =
        flip List.map values @ fun (field_name, (Any_t_opt_value (t, value))) ->
          let content = aux_form ?value name t in
          tr [
            td ~a:[a_class [label_class]]
              [pcdata field_name];
            td ~a:[a_class [ record_field_marker_class field_name ]]
              [ content ];
          ]
      in
      Html5.D.table ~a:(a_class[form_class; record_class] :: a_local)
        (List.hd rows) (List.tl rows)

  and aux_list_item : 'a . ?value:'a -> string -> 'a Deriving_Typerepr.t -> Html5_types.li elt =
    fun (type a) ?(value:a option) name (t : a t) ->
      let remove =
        Html5.D.Raw.a ~a:[a_class [button_remove_class]] [
          span ~a:[a_class [label_class]] [pcdata "remove"];
          marker;
        ]
      in
      let item =
        Html5.D.li ~a:[a_class [list_item_class]] [
          aux_form ?value name t;
          remove
        ]
      in
      ignore {unit{
        onload_or_now @ fun () ->
          Lwt.async @ fun () ->
            let remove = Html5.To_dom.of_element %remove in
            let item = Html5.To_dom.of_li %item in
            Lwt_js_events.clicks remove @ fun _ _ ->
              let _outmost =
                Option.get' (fun () -> Eliom_lib.error_any item "No outmost") @
                  parent_with_class form_outmost_class item
              in
              let parent = Js.Opt.get (item ## parentNode) @ fun () -> failwith "No parent" in
              Dom.removeChild parent item;
              Lwt.return_unit
      }};
      item

  and aux_form : 'a . ?is_outmost:bool -> ?value:'a -> string -> 'a t -> form_content elt =
    fun (type a) ?(is_outmost=false) ?(value: a option) name (t : a t) ->
      let a_local =
        if is_outmost then
          Some [ a_class [ form_outmost_class ] ]
        else None
      in
      let a_atomic = a_class [form_class; atomic_class] :: Option.get [] a_local in
      match t with
        | String ->
          marked ~a:a_atomic @ raw_input ~input_type:`Text ?value ~name ()
        | Int ->
          let value = Option.map string_of_int value in
          marked ~a:a_atomic @ raw_input ~a:[a_step @ `Step 1.0] ~input_type:`Number ?value ~name ()
        | Int32 ->
          let value = Option.map Int32.to_string value in
          marked ~a:a_atomic @ raw_input~a:[a_step @ `Step 1.0]  ~input_type:`Number ?value ~name ()
        | Int64 ->
          let value = Option.map Int64.to_string value in
          marked ~a:a_atomic @ raw_input ~a:[a_step @ `Step 1.0] ~input_type:`Number ?value ~name ()
        | Float ->
          let value = Option.map string_of_float value in
          marked ~a:a_atomic @ raw_input ~a:[a_step `Any] ~input_type:`Number ?value ~name ()
        | Bool ->
          marked ~a:a_atomic @ raw_checkbox ?checked:value ~name ~value:"" ()
        | Option t ->
          aux_form_option ?value ?a_local name t
        | List t ->
          aux_form_list ?value ?a_local name t
        | Tuple tuple ->
          aux_form_tuple ?a_local ?value name tuple
        | Sum sum ->
          aux_form_sum ?a_local ?value name sum
        | Record record ->
          aux_form_record ?a_local ?value name record
        | Unit -> Html5.D.span ~a:a_atomic []
        | Array _ ->
          failwith "Generate_form.form: not for array"
        | Function _ ->
          failwith "Generate_form.form: not for functions"
        | Ref _ ->
          failwith "Generate_form.form: not for refs"

  and aux_form_option : 'a . ?value:'a option -> ?a_local:_  -> string -> 'a t -> form_content elt =
    let open Html5.F in
    fun ?value ?(a_local=[]) name t ->
      let selector =
        let checked = Option.map ((<>) None) value in
        let a = [a_class[option_selector_class]] in
        Html5.D.raw_checkbox ~a ?checked ~name:"" ~value:"" ()
      in
      let content =
        let value = Option.bind value identity in
        let a = [a_class [option_content_class]] in
        div ~a [ aux_form ?value name t ]
      in
      ignore {unit{
        onload_or_now @ fun () ->
          Lwt.async @ fun () ->
            let selector = Html5.To_dom.of_input %selector in
            Lwt_js_events.changes selector @ fun _ _ ->
              Lwt.return @
                match parent_with_class form_outmost_class selector with
                  | Some outmost -> reset_required outmost
                  | None -> failwith "Generate_form: no outmost"
      }};
      let a = a_class [form_class; option_class] :: (a_local :> Html5_types.div_attrib attrib list) in
      Html5.D.div ~a [ marked selector; content]

  and aux_form_list : 'a . ?value:'a list -> ?a_local:_  -> name -> 'a t -> form_content elt =
    fun (type a) ?(value : a list option) ?(a_local=[]) name (t : a t) ->
      let add =
        Html5.D.Raw.a ~a:[a_class [button_add_class]] [
          span ~a:[a_class [label_class]] [pcdata "add"];
          marker
        ]
      in
      let add_li = Html5.D.li [ add ] in
      let content =
        Html5.D.ul @
          (match value with
            | None ->
              []
            | Some values ->
              flip List.map values @
                fun value ->
                  aux_list_item ~value name t) @@
          [ add_li ]
      in
      ignore {unit{
        onload_or_now @ fun () ->
          Lwt.async @ fun () ->
            let add = Html5.To_dom.of_element %add in
            let content = Html5.To_dom.of_element %content in
            Lwt_js_events.clicks add @ fun _ _ ->
              let item = !aux_list_item_ref %name %(Any_t t) in
              Html5.Manip.appendChild ~before:%add_li %content item;
              reset_required @
                Option.get' (fun () -> Eliom_lib.error_any item "No outmost'") @
                parent_with_class form_outmost_class content;
              Lwt.return ()
      }};
      let a = a_class [form_class; list_class] :: (a_local :> Html5_types.div_attrib attrib list) in
      Html5.D.div ~a [content]

  let content : 'a . 'a t -> ?value:'a ->
    [ `One of 'a Eliom_parameter.caml ] Eliom_parameter.param_name -> form_content elt =
    fun (type a) (t : a Deriving_Typerepr.t) ?(value : a option) name ->
      let name = (Obj.magic name : string) in
      let content = aux_form ~is_outmost:true ?value name t in
      ignore {unit{
        onload_or_now @ fun () ->
          let content = Html5.To_dom.of_element %content in
          reset_required content;
          flip Option.iter (parent Dom_html.(tagged |- function Form _ -> true | _ -> false) content) @
            fun form ->
              let form =
                Js.Opt.get (Dom_html.CoerceTo.form form)
                  (fun () -> assert false)
              in
              form ## onsubmit <- Dom_html.handler @ fun _ ->
                trace "SUBMIT";
                (try
                   submit_form %name %(Deriving_Typerepr.Any_t t) form
                 with exn ->
                   Eliom_lib.debug_exn "Error while submitting" exn);
                Js._false
      }};
      content
}}

{client{
  let () =
    aux_list_item_ref := fun name (Any_t t) -> aux_list_item ?value:None name t
}}
