

{shared{

  let css_filename = "eliom-form-generator.css"

  open Html5_types
  open Eliom_content
  open Printf
  type 'a elt = 'a Eliom_content.Html5.elt

  external identity : 'a -> 'a = "%identity"
  external (@) : ('a -> 'b) -> 'a -> 'b = "%apply"
  let (@@) = List.append
  let (|-) f g x = g (f x)
  let (-|) f g x = f (g x)
  let const x _ = x
  let flip f x y = f y x
  let flip2 f x y z = f z x y
  module List = struct
    include List
    let cons x xs = x :: xs
    let index_of xs x =
      let rec aux ix = function
        | [] -> raise Not_found
        | x' :: _ when x = x' -> ix
        | _ :: xs -> aux (succ ix) xs
      in
      aux 0 xs
  end
  module Option = struct
    let default_delayed f = function
      | Some x -> x
      | None -> f ()
    let map f = function
      | Some x -> Some (f x)
      | None -> None
    let bind x f =
      match x with
        | Some x -> f x
        | None -> None
    let default x = function
      | Some x -> x
      | None -> x
    let is_some x = x <> None
    let of_list = function
      | [] -> None
      | x :: _ -> Some x
    let may f = function
      | Some x -> f x
      | None -> ()
    let plus o1 o2 =
      if is_some o1 then o1 else o2
    let merge f o1 o2 =
      match o1, o2 with
        | Some x1, Some x2 -> f x1 x2
        | None, only | only, None -> only
  end
  let cons_if p x =
    if p x
    then List.cons x
    else identity
  let before f x = f x; x

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

}}
{server{
  let trace fmt = ksprintf (fun str -> Ocsigen_messages.console @ fun () -> str) fmt
}}

{shared{

  let form_outmost_class = "eliom-form-outmost"
  let form_class = "eliom-form"
  let form_annotation_class = "eliom-form-annotation"
  let atomic_class = "eliom-form-atomic"
  let option_class = "eliom-form-option"
  let option_selector_class = "eliom-form-option-selector"
  let option_content_class = "eliom-form-option-content"
  let sum_class = "eliom-form-sum"
  let sum_selector_class = "eliom-form-sum-selector"
  let sum_content_class = "eliom-form-sum-content"
  let sum_case_class = "eliom-form-sum-case"
  let sum_case_marker_class case = sum_case_class ^ "-" ^ case
  let variant_class = "eliom-form-variant"
  let variant_selector_class = "eliom-form-variant-selector"
  let variant_content_class = "eliom-form-variant-content"
  let variant_case_class = "eliom-form-variant-case"
  let variant_case_marker_class case = variant_case_class ^ "-" ^ case
  let tuple_class = "eliom-form-tuple"
  let record_class = "eliom-form-record"
  let record_field_marker_class field = "eliom-form-record-field-"^field
  let label_class = "eliom-form-label"
  let annotation_class = "eliom-form-annotation"
  let marker_class = "eliom-form-input-marker"
  let selector_snippet = "selector"
  let content_snippet = "content"
  let list_class = "eliom-form-list"
  let list_item_class = "eliom-form-list-item"
  let array_class = "eliom-form-array"
  let array_item_class = "eliom-form-array-item"
  let button_add_class = "eliom-form-add-elt"
  let button_remove_class = "eliom-form-remove-elt"
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
      Option.default_delayed (fun () -> Eliom_lib.error_any node "is_required") @
        control_element node
    with
      | `Input input ->
        if input ## _type <> Js.string "checkbox" then
          input ## required <- Js.bool is_required
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
        atomic_class, `Atomic ;
        variant_class, `Variant ;
        array_class, `Array ;
      ]
    with
      | [_, category] -> category
      | [] -> Eliom_lib.error_any node "No categories"
      | _ -> Eliom_lib.error_any node "Many categories"

  let querySelector ?(not_between=fun _ -> false) node fmt =
    flip ksprintf fmt @ fun selector ->
      trace_any node "querySelector: %s" selector;
      let node = (node :> Dom_html.element Js.t) in
      let nodes = Dom.list_of_nodeList (node ## querySelectorAll (Js.string selector)) in
      Option.of_list @
        flip List.filter nodes @ fun node' ->
          not @ List.exists not_between @
            path ~inclusive_from:false ~inclusive_to:false
              node' node

  let querySelectorAll ?(not_between=fun _ -> false) node fmt =
    flip ksprintf fmt @ fun selector ->
      trace_any node "querySelectorAll: %s" selector;
      let node = (node :> Dom_html.element Js.t) in
      let nodes = Dom.list_of_nodeList (node ## querySelectorAll (Js.string selector)) in
      flip List.filter nodes @ fun node' ->
        not @ List.exists not_between @
          path ~inclusive_from:false ~inclusive_to:false
            node' node

  let option_selector form =
    trace_any form "option_selector";
    if not @ has_class form option_class then
      Eliom_lib.error_any form "option_selector: not an option";
    let input =
      Option.default_delayed (fun () -> Eliom_lib.error_any form "Generate_form.option_selector: No selector") @
        querySelector ~not_between:(flip has_class form_class) form ".%s" option_selector_class
    in
    Js.Opt.get (Dom_html.CoerceTo.input input)
      (fun () -> Eliom_lib.error_any input "Generate_form.option_selector: Not input")

  let option_content form : #Dom_html.element Js.t =
    trace_any form "option_content";
    if not @ has_class form option_class then
      Eliom_lib.error_any form "option_content: not an option";
    Option.default_delayed (fun () -> Eliom_lib.error_any form "Generate_form.option_selector: No content") @
      querySelector ~not_between:(flip has_class form_class) form ".%s" option_content_class

  let sum_selector form =
    trace_any form "sum_selector";
    if not @ has_class form sum_class then
      Eliom_lib.error_any form "sum_selector: not a sum";
    let select =
      Option.default_delayed (fun () -> Eliom_lib.error_any form "Generate_form.sum_selector: No selector") @
        querySelector ~not_between:(flip has_class form_class) form ".%s" sum_selector_class
    in
    Js.Opt.get (Dom_html.CoerceTo.select select)
      (fun () -> Eliom_lib.error_any select "Generate_form.sum_selector: Not select")

  let sum_case form case =
    trace_any form "sum_case %s" case;
    if not @ has_class form sum_class then
      Eliom_lib.error_any form "sum_case: not a sum";
    Option.default_delayed (fun () -> Eliom_lib.error_any form "Generate_form.sum_selector: No case %s" case) @
      querySelector ~not_between:(flip has_class form_class) form ".%s" (sum_case_marker_class case)

  let sum_cases form =
    trace_any form "sum_cases";
    if not @ has_class form sum_class then
      Eliom_lib.error_any form "sum_cases: not a sum";
    querySelectorAll ~not_between:(flip has_class form_class) form ".%s" sum_case_class

  let variant_selector form =
    trace_any form "variant_selector";
    if not @ has_class form variant_class then
      Eliom_lib.error_any form "variant_selector: not a variant";
    let select =
      Option.default_delayed (fun () -> Eliom_lib.error_any form "Generate_form.variant_selector: No selector") @
        querySelector ~not_between:(flip has_class form_class) form ".%s" variant_selector_class
    in
    Js.Opt.get (Dom_html.CoerceTo.select select)
      (fun () -> Eliom_lib.error_any select "Generate_form.variant_selector: Not select")

  let variant_case form case =
    trace_any form "variant_case %s" case;
    if not @ has_class form variant_class then
      Eliom_lib.error_any form "variant_case: not a variant";
    Option.default_delayed (fun () -> Eliom_lib.error_any form "Generate_form.variant_selector: No case %s" case) @
      querySelector ~not_between:(flip has_class form_class) form ".%s" (variant_case_marker_class case)

  let variant_cases form =
    trace_any form "variant_cases";
    if not @ has_class form variant_class then
      Eliom_lib.error_any form "variant_cases: not a variant";
    querySelectorAll ~not_between:(flip has_class form_class) form ".%s" variant_case_class

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
          | `Tuple | `Record | `List | `Array | `Atomic -> true
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
                Js.Opt.test (case_content ## querySelector (js_string ".%s" class_id))
          | `Variant ->
            let case = Js.to_string @ (variant_selector parent) ## value in
            if case = "" then
              false
            else
              let case_content = variant_case parent case in
              with_class_id form @ fun class_id ->
                Js.Opt.test (case_content ## querySelector (js_string ".%s" class_id))

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

  let set_variant_hidden form =
    trace_any form "set_variant_hidden";
    if not @ has_class form variant_class then
      Eliom_lib.error_any form "set_variant_hidden";
    let case = Js.to_string @ (variant_selector form) ## value in
    flip List.iter (variant_cases form) @ fun case_content ->
      if has_class case_content (variant_case_marker_class case) then
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
    is_required_form node

  let set_required_atomic (node : Dom_html.element Js.t) required =
    trace_any node "set_required_atomic";
    if not @ has_class node atomic_class then
      Eliom_lib.error_any node "set_required_atomic";
    Js.Opt.iter
      (node ## querySelector (js_string "input, textarea, select"))
      (flip set_required required)

  let is_required_sum_selector node =
    trace_any node "is_required_sum_selector";
    if not @ has_class node sum_selector_class then
      Eliom_lib.error_any node "is_required_sum_selector";
    let form =
      Option.default_delayed (fun () -> Eliom_lib.error_any node "is_required_sum_selector: not within generated form") @
        parent_with_class form_class node
    in
    is_required_form form

  let set_required_sum_selector node required =
    trace_any node "set_required_sum_selector";
    if not @ has_class node sum_selector_class then
      Eliom_lib.error_any node "set_required_sum_selector";
    set_required node required

  let set_required_variant_selector node required =
    trace_any node "set_required_variant_selector";
    if not @ has_class node variant_selector_class then
      Eliom_lib.error_any node "set_required_variant_selector";
    set_required node required

  let is_required_variant_selector node =
    trace_any node "is_required_variant_selector";
    if not @ has_class node variant_selector_class then
      Eliom_lib.error_any node "is_required_variant_selector";
    let form =
      Option.default_delayed (fun () -> Eliom_lib.error_any node "is_required_variant_selector: not within generated form") @
        parent_with_class form_class node
    in
    is_required_form form

  let reset_required (outmost : #Dom_html.element Js.t) =
    trace_any outmost "reset_required";
    if not @ has_class outmost form_outmost_class then
      Eliom_lib.error_any outmost "reset_required: not outmost";

    List.iter (fun input -> set_required_atomic input @ is_required_atomic input) @
      before (Eliom_lib.trace "%i atomics to reset required" -| List.length) @
        Dom.list_of_nodeList (outmost ## querySelectorAll (js_string ".%s" atomic_class));

    List.iter (fun input -> set_required_sum_selector input @ is_required_sum_selector input) @
      before (Eliom_lib.trace "%i sum selectors to reset required" -| List.length) @
        Dom.list_of_nodeList (outmost ## querySelectorAll (js_string ".%s" sum_selector_class));

    List.iter (fun input -> set_required_variant_selector input @ is_required_variant_selector input) @
      before (Eliom_lib.trace "%i variant selectors to reset required" -| List.length) @
        Dom.list_of_nodeList (outmost ## querySelectorAll (js_string ".%s" variant_selector_class));

    List.iter set_sum_hidden @
      before (Eliom_lib.trace "%i sums" -| List.length) @
        cons_if (flip has_class sum_class) outmost @
          Dom.list_of_nodeList (outmost ## querySelectorAll (js_string ".%s" sum_class));

    List.iter set_variant_hidden @
      before (Eliom_lib.trace "%i variants" -| List.length) @
        cons_if (flip has_class variant_class) outmost @
          Dom.list_of_nodeList (outmost ## querySelectorAll (js_string ".%s" variant_class));

    List.iter set_option_hidden @
      before (Eliom_lib.trace "%i options" -| List.length) @
        cons_if (flip has_class option_class) outmost @
          Dom.list_of_nodeList (outmost ## querySelectorAll (js_string ".%s" option_class));

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
        fun (type a) node tuple ->
          let category = categorize_form node in
          if category <> `Tuple then
            Eliom_lib.error_any node "data_from_form: not tuple (composed)";
          let component_contents =
            Dom.list_of_nodeList (node ## childNodes)
          in
          create_tuple tuple
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
            Option.default_delayed (fun () -> assert false) @
              control_element @
                Option.default_delayed (fun () -> Eliom_lib.error_any node "data_from_form: atomic without control") @
                  querySelector node "input, textarea, select"
          in
          match control with
            | `Input input -> input ## value
            | `Textarea textarea -> textarea ## value
            | `Select select -> select ## value
            | `Fieldset _ -> assert false
      in
      match (t : a t) with
        | Atomic Unit ->
          if category <> `Atomic then
            Eliom_lib.error_any node "data_from_form: unit not atomic";
          (() : a)
        | Atomic String -> identity @ get_atomic_string "string"
        | Atomic Int -> int_of_string @ get_atomic_string "int"
        | Atomic Float -> float_of_string @ get_atomic_string "float"
        | Atomic Int32 -> Int32.of_string @ get_atomic_string "int32"
        | Atomic Int64 -> Int64.of_string @ get_atomic_string "int64"
        | Atomic Bool ->
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
          let ol =
            Js.Opt.get
              (Js.Opt.bind
                 (Js.Opt.bind
                    (node ## childNodes ## item (0))
                    Dom_html.CoerceTo.element)
                 Dom_html.CoerceTo.ol) @
              fun () -> Eliom_lib.error_any node "data_from_form: first element not an ol"
          in
          let items =
            flip List.map (Dom.list_of_nodeList @ ol ## childNodes) @ fun li ->
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
        | Array t ->
          if category <> `Array then
            Eliom_lib.error_any node "data_from_form: not array";
          let ol =
            Js.Opt.get
              (Js.Opt.bind
                 (Js.Opt.bind
                    (node ## childNodes ## item (0))
                    Dom_html.CoerceTo.element)
                 Dom_html.CoerceTo.ol) @
              fun () -> Eliom_lib.error_any node "data_from_form: first element not an ol"
          in
          let items =
            flip List.map (Dom.list_of_nodeList @ ol ## childNodes) @ fun li ->
              let li =
                Js.Opt.get (Js.Opt.bind (Dom_html.CoerceTo.element li) Dom_html.CoerceTo.li) @
                  fun () -> Eliom_lib.error_any li "data_from_form: not a li"
              in
              if has_class li array_item_class then
                Some
                  (Js.Opt.get (li ## childNodes ## item (0)) @
                     fun () -> Eliom_lib.error_any li "data_from_form: no li content")
              else None
          in
          let items =
            List.map (function Some item -> item | None -> assert false) @
              List.filter ((<>) None) items
          in
          Array.of_list @ List.map (flip data_from_form t) items
        | Sum { summands } ->
          if category <> `Sum then
            Eliom_lib.error_any node "data_from_form: sum";
          let selector = sum_selector node in
          let case = Js.to_string @ selector ## value in
          if case = "" then
            Eliom_lib.error_any node "data_from_form: no case selected for sum";
          let Any_summand summand = List.assoc case summands in
          (match summand with
            | Summand_nullary _ as summand ->
              create_sum_case summand ()
            | Summand_unary unary_summand as summand ->
              let _, t = (unary_summand : (_, _) unary :> _ * _) in
              let content =
                Js.Opt.get (sum_case node case) ## childNodes ## item(0) @
                  fun () -> Eliom_lib.error_any  node "data_from_form: no elt for sum case %S" case
              in
              create_sum_case summand @ data_from_form content t
            | Summand_nary nary_summand as summand ->
              let _, tuple = (nary_summand : (_, _) nary :> _ * _) in
              let content =
                Js.Opt.get
                  (Js.Opt.bind
                     ((sum_case node case) ## childNodes ## item (0))
                     Dom_html.CoerceTo.element) @
                  fun () -> Eliom_lib.error_any  node "data_from_form: no tuple for sum case %S" case
              in
              create_sum_case summand @ aux_tuple content tuple)
        | Variant { tagspecs } ->
          if category <> `Variant then
            Eliom_lib.error_any node "data_from_form: variant";
          let selector = variant_selector node in
          let case = Js.to_string @ selector ## value in
          if case = "" then
            Eliom_lib.error_any node "data_from_form: no case selected for variant";
          let Any_tagspec tagspec = List.assoc case tagspecs in
          (match tagspec with
            | Tag_nullary _ as tag ->
              create_variant_case tag ()
            | Tag_unary unary as tag ->
              let _, t = (unary : (_, _) unary :> _ * _) in
              let content =
                Js.Opt.get (variant_case node case) ## childNodes ## item(0) @
                  fun () -> Eliom_lib.error_any  node "data_from_form: no elt for variant case %S" case
              in
              create_variant_case tag @ data_from_form content t
            | Tag_nary nary as tag ->
              let _, tuple = (nary : (_, _) nary :> _ * _) in
              let content =
                Js.Opt.get
                  (Js.Opt.bind
                     ((variant_case node case) ## childNodes ## item (0))
                     Dom_html.CoerceTo.element) @
                  fun () -> Eliom_lib.error_any  node "data_from_form: no tuple for variant case %S" case
              in
              create_variant_case tag @ aux_tuple content tuple)
        | Record ({ fields } as record) ->
          if category <> `Record then
            Eliom_lib.error_any node "data_from_form: not record";
          let field_contents =
            flip List.map fields @ fun (name, _) ->
              Option.default_delayed (fun () -> Eliom_lib.error_any node "data_from_form: no field content for %S" name) @
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
          create_record record f
        | Function _ ->
          failwith "Generate_form.data_from_form: function"
        | Ref _ ->
          failwith "Generate_form.data_from_form: ref"

 let name_any_t_custom_data =
   Eliom_content.Html5.Custom_data.create
     ~name:"name_any_t"
     ~to_string:(Eliom_lib.to_json ?typ:None)
     ~of_string:(Eliom_lib.of_json ?typ:None)
     ()

 let init_form name any_t (form : Dom_html.formElement Js.t) =
   form ## onsubmit <- Dom_html.handler @ fun _ ->
     trace "SUBMIT";
     (try
        let open Deriving_Typerepr in
        match any_t with
          | Any_t t ->
            let content =
              flip Option.default_delayed (querySelector form ".%s" form_outmost_class) @
                fun () -> Eliom_lib.error_any form "submit_for: No outmost form"
            in
            let value = data_from_form (content :> Dom.node Js.t) t in
            Eliom_lib.debug "submit_form: %s (%s)"
              (show t value) (Eliom_lib.to_json value);
            let form' =
              Js.Opt.get
                (Js.Opt.bind
                   (Dom_html.CoerceTo.element @
                      form ## cloneNode (Js._true))
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
            Dom.appendChild form' (Html5.To_dom.of_node input);
            ignore (form' ## onsubmit <- Eliom_client.form_handler);
            form' ## style ## display <- js_string "none";
            Dom.appendChild (Dom_html.document ## body) form';
            form' ## submit ();
            Dom.removeChild (Dom_html.document ## body) form'
      with exn ->
        Eliom_lib.debug_exn "Error while submitting" exn);
     Js._false
 }}

{shared{
  let aux_list_item_ref : (int -> string -> Deriving_Typerepr.any_t -> Html5_types.li elt) ref =
    ref @ fun _ _ _ -> failwith "aux_list_item_ref"
}}

{shared{

  open Html5.F
  open Deriving_Typerepr

  type 'a any_field_opt_value =
    | Any_field_opt_value : ('a, 'b) field * 'b option -> 'a any_field_opt_value

  type 'a value = [ `Default of 'a | `Constant of 'a | `Hidden of 'a ]
  module Value = struct
    let kind = function
      | `Default _ -> fun x -> `Default x
      | `Constant _ -> fun x -> `Constant x
      | `Hidden _ -> fun x -> `Hidden x
    let get = function
      | `Default x -> x
      | `Constant x -> x
      | `Hidden x -> x
    let map =
      fun f v ->
        kind v @ f @ get v
    let put_over_option =
      fun v ->
        match get v with
          | Some a -> Some (kind v a)
          | None -> None
  end

  type 'a widget_fun =
    ?value:'a value ->
    'a Eliom_parameter.setoneradio Eliom_parameter.param_name ->
    Html5_types.span_content Eliom_content.Html5.elt

  type 'a template =
    | Widget : 'a atomic * 'a widget_fun -> 'a template

  let string_widget f =
    Widget (String, f)

  type 'a config = {
    value : 'a value option;
    label : string option;
    annotation : string option;
    a : Html5_types.div_attrib Eliom_content.Html5.F.attrib list;
    template : 'a template option
  }
  module Config = struct
    let zero = { value = None ; label = None ; annotation = None ; a = [] ; template = None}
    let plus c1 c2 =
      { value = Option.plus c1.value c2.value ;
        label = Option.plus c1.label c2.label ;
        annotation = Option.plus c1.annotation c2.annotation ;
        a = List.append c1.a c2.a ;
        template = Option.plus c1.template c2.template }
  end

  type 'a any_path = Any_path : ('a, _) p -> 'a any_path
  type any_config = Any_config : 'c config -> any_config

  module Configs : sig
    type 'a configs
    val zero : 'a configs
    val add : ('a, 'b) p -> 'b config -> 'a configs -> 'a configs
    val find : ('a, 'b) p -> 'a configs -> 'b config
    val plus : 'a configs -> 'a configs -> 'a configs
  end = struct
    type 'a configs = ('a any_path * any_config) list
    let zero = []
    let find (type b) (p : (_, b) p) cs =
      try
        let Any_config config = List.assoc (Any_path p) cs in
        (Obj.magic config : b config)
      with Not_found -> Config.zero
    let add p c cs =
      let c0 = find p cs in
      let c = Config.plus c c0 in
      (Any_path p, Any_config c) :: List.remove_assoc (Any_path p) cs
    let plus (type w) (type a) cs1 cs2 =
      let
        module Map = Map.Make
          (struct
            type t = a any_path
            let compare = Pervasives.compare
           end)
      in
      let map_from_list li =
        List.fold_right (fun (x, y) -> Map.add x y) li Map.empty
      in
      let cs1 = map_from_list cs1 in
      let cs2 = map_from_list cs2 in
      let merger (type b) _ c1 c2 =
        let force (Any_config c) = (Obj.magic c : b config) in
        Option.map (fun c -> Any_config c) @
          match Option.map force c1, Option.map force c2 with
            | Some c1, Some c2 -> Some (Config.plus c1 c2)
            | None, only | only, None -> only
      in
      Map.bindings @ Map.merge merger cs1 cs2
  end

  let configs_find_with_value configs path value =
    let c = Configs.find configs path in
    let c = Config.plus c { Config.zero with value } in
    match c.value with
      | Some (`Hidden value) ->
        { c with a = a_hidden `Hidden :: c.a }
      | _ -> c

  type 'a configs = 'a Configs.configs

  let paths : type a . a -> a t -> a any_path list =
    fun value t ->
      let folder : type b . a any_path list -> b -> b t -> (a, b) p -> a any_path list =
        fun sofar _ _ path ->
          Any_path path :: sofar
      in
      fold { folder } [] value t

  let from_value : type a b . (a, b) p -> b value -> b t -> a configs =
    fun original_path value t ->
      let folder configs (Any_path path) =
        Option.default configs @
          flip Option.map (get (Value.get value) path) @ fun value' ->
            let config = { Config.zero with value = Some (Value.kind value value') } in
            let path = Deriving_Typerepr.compose path original_path in
            Configs.add path config configs
      in
      List.fold_left folder Configs.zero @ paths (Value.get value) t

  let rec aux_form_tuple : type a b . a configs -> b value option -> (a, b) p -> string -> b tuple -> form_content elt =
    fun configs value path name { components } ->
      let { value ; a ; label ; annotation ; template } = configs_find_with_value path configs value in
      assert (template = None);
      Html5.D.div ~a:(a_class [ form_class ; tuple_class ] :: a) @
        flip List.map components @ fun (Any_component (Component (t, _) as component)) ->
          let value = flip Option.map value (Value.map @ get_tuple_component component) in
          let path = Tuple_component (component, path) in
          aux_form configs value path name t

  and aux_form_sum  : type a b . a configs -> b value option -> (a, b) p -> string -> b sum -> form_content elt =
    fun configs value path name { summands } ->
      let { value ; a ; label ; annotation ; template } = configs_find_with_value path configs value in
      assert (template = None);
      let annotation =
        Option.default [] @
          flip Option.map annotation @ fun annotation ->
            [ Html5.F.div ~a:[a_class [form_annotation_class]]
                [pcdata annotation] ]
      in
      let selector =
        let a = [ a_class [ sum_selector_class ] ] in
        let null = Html5.D.Option ([], "", Some (pcdata "- select -"), value = None) in
        let summands =
          flip List.map summands @ fun (summand_name, Any_summand summand) ->
            let value = flip Option.map value (get_sum_case_by_summand summand -| Value.get) in
            let selected =
              Option.default false @
                Option.map (Option.default false) @
                  Option.map (Option.map (const true)) value
            in
            let label =
              match summand with
                | Summand_nullary summand ->
                  let path = Case_nullary (summand, path) in
                  (Configs.find path configs).label
                | Summand_unary summand ->
                  let path = Case_unary (summand, path) in
                  (Configs.find path configs).label
                | Summand_nary summand ->
                  let path = Case_nary (summand, path) in
                  (Configs.find path configs).label
            in
            let label = Option.default summand_name label in
            Html5.D.Option ([], summand_name, Some (pcdata label), selected)
        in
        Html5.D.raw_select ~a ~name null summands
      in
      let content =
        div ~a:[a_class [sum_content_class]] @
          flip List.map summands @ fun (summand_name, Any_summand summand) ->
            div ~a:[a_class [sum_case_class; sum_case_marker_class summand_name]] @
              let value =
                Option.bind value @ fun value ->
                  match get_sum_case_by_summand summand (Value.get value) with
                    | None -> None
                    | Some value' -> Some (Value.kind value value')
              in
              match summand with
                | Summand_nullary _ -> []
                | Summand_unary unary_summand ->
                  let _, t = (unary_summand : (_, _) unary :> _ * _) in
                  let path = Case_unary (unary_summand, path) in
                  [ aux_form configs value path name t ]
                | Summand_nary nary_summand ->
                  let (_, t) = (nary_summand : (_, _) nary :> _ * _) in
                  let path = Case_nary (nary_summand, path) in
                  [ aux_form_tuple configs value path name t ]
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
      Html5.D.div ~a:(a_class [form_class; sum_class] :: a) @
        [span [selector; marker]; content] @@ annotation

  and aux_form_variant  : type a b . a configs -> b value option -> (a, b) p -> string -> b variant -> form_content elt =
    fun configs value path name { tagspecs } ->
      let { value ; a ; label ; annotation ; template } = configs_find_with_value path configs value in
      assert (template = None);
      let annotation =
        Option.default [] @
          flip Option.map annotation @ fun annotation ->
            [ Html5.F.div ~a:[a_class [form_annotation_class]]
                [pcdata annotation] ]
      in
      let selector =
        let a = [ a_class [ variant_selector_class ] ] in
        let null = Html5.D.Option ([], "", Some (pcdata "- select -"), value = None) in
        let tagspecs =
          flip List.map tagspecs @ fun (tagspec_name, Any_tagspec tagspec) ->
            let value = flip Option.map value (get_variant_case_by_tagspec tagspec -| Value.get) in
            let selected =
              Option.default false @
                Option.map (Option.default false) @
                  Option.map (Option.map (const true)) value
            in
            let label =
              match tagspec with
                | Tag_nullary nullary ->
                  let path = Variant_case_nullary (nullary, path) in
                  (Configs.find path configs).label
                | Tag_unary unary ->
                  let path = Variant_case_unary (unary, path) in
                  (Configs.find path configs).label
                | Tag_nary nary ->
                  let path = Variant_case_nary (nary, path) in
                  (Configs.find path configs).label
            in
            let label = Option.default tagspec_name label in
            Html5.D.Option ([], tagspec_name, Some (pcdata label), selected)
        in
        Html5.D.raw_select ~a ~name null tagspecs
      in
      let content =
        div ~a:[a_class [variant_content_class]] @
          flip List.map tagspecs @ fun (tagspec_name, Any_tagspec tagspec) ->
            div ~a:[a_class [variant_case_class; variant_case_marker_class tagspec_name]] @
              let value =
                Option.bind value @ fun value ->
                  match get_variant_case_by_tagspec tagspec (Value.get value) with
                    | None -> None
                    | Some value' -> Some (Value.kind value value')
              in
              match tagspec with
                | Tag_nullary _ -> []
                | Tag_unary unary ->
                  let _, t = (unary : (_, _) unary :> _ * _) in
                  let path = Variant_case_unary (unary, path) in
                  [ aux_form configs value path name t ]
                | Tag_nary nary ->
                  let (_, t) = (nary : (_, _) nary :> _ * _) in
                  let path = Variant_case_nary (nary, path) in
                  [ aux_form_tuple configs value path name t ]
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
      Html5.D.div ~a:(a_class [form_class; variant_class] :: a) @
        [span [selector; marker]; content ] @@ annotation

  and aux_form_record : type a b . a configs -> b value option -> (a, b) p -> string -> b record -> form_content elt =
    fun configs value path name { fields } ->
      let { value ; a ; label ; annotation ; template } = configs_find_with_value path configs value in
      assert (template = None);
      let annotation =
        Option.default [] @
          flip Option.map annotation @ fun annotation ->
            [ Html5.F.tr
                [ Html5.F.td [];
                  Html5.F.td ~a:[a_class [form_annotation_class]]
                    [pcdata annotation] ] ]
      in
      let rows =
        flip List.map fields @ fun (field_name, Any_field (Field (_, t) as field)) ->
          let path = Record_field (field, path) in
          let value = flip Option.map value @ Value.map @ get_record_field field in
          let { a ; label ; annotation ; value ; _ } = configs_find_with_value path configs value in
          let label = Option.default field_name label in
          let annotation = Option.default "" annotation in
          let content = aux_form configs value path name t in
          tr ~a [
            td ~a:[a_class [label_class]]
              [pcdata label];
            td ~a:[a_class [record_field_marker_class field_name]]
              [content];
            td ~a:[a_class [annotation_class]]
              [pcdata annotation]
          ]
      in
      let rows = rows @@ annotation in
      Html5.D.table ~a:(a_class[form_class; record_class] :: a)
        (List.hd rows) (List.tl rows)

  and aux_list_item : type a b . a configs -> b value option -> (a, b) p -> int -> string -> b Deriving_Typerepr.t -> Html5_types.li elt =
    fun configs value path _ix name t ->
      let remove =
        Html5.D.Raw.a ~a:[a_class [button_remove_class]] [
          span ~a:[a_class [label_class]] [pcdata "remove"];
          marker;
        ]
      in
      let item =
        let { value ; a ; label ; annotation ; template } = configs_find_with_value path configs value in
        let a = a_class [list_item_class] :: a in
        Html5.D.li ~a [
          aux_form configs value path name t;
          remove;
        ]
      in
      ignore {unit{
        onload_or_now @ fun () ->
          Lwt.async @ fun () ->
            let remove = Html5.To_dom.of_element %remove in
            let item = Html5.To_dom.of_li %item in
            Lwt_js_events.clicks remove @ fun _ _ ->
              let _outmost =
                Option.default_delayed (fun () -> Eliom_lib.error_any item "No outmost") @
                  parent_with_class form_outmost_class item
              in
              let parent = Js.Opt.get (item ## parentNode) @ fun () -> failwith "No parent" in
              Dom.removeChild parent item;
              Lwt.return_unit
      }};
      item

  and aux_array_item : type a b . a configs -> b value option -> (a, b) p -> int -> string -> b Deriving_Typerepr.t -> Html5_types.li elt =
    fun configs value path _ix name t ->
      let { value ; a ; label ; annotation ; template } = configs_find_with_value path configs value in
      let a = a_class [array_item_class] :: a in
      Html5.D.li ~a [
        aux_form configs value path name t;
      ]

  and aux_atomic : type a b . a configs -> b value option -> (a, b) p -> string -> b atomic -> form_content elt =
    fun configs value path name atomic ->
      let { value ; a ; label ; annotation ; template } = configs_find_with_value path configs value in
      let a = a_class [form_class; atomic_class] :: a in
      marked ~a @
        match template with
          | Some (Widget (atomic', widget)) when eq_atomic atomic atomic' ->
            widget ?value (Obj.magic name)
          | _ ->
            let a =
              match value with
                | Some (`Constant _) -> [a_readonly `ReadOnly]
                | _ -> []
            in
            let value = Option.map Value.get value in
            match atomic with
              | Unit -> Html5.D.span []
              | String ->
                raw_input ~a ~input_type:`Text ?value ~name ()
              | Int ->
                let value = Option.map string_of_int value in
                raw_input ~a ~input_type:`Text ?value ~name ()
              | Int32 ->
                let value = Option.map Int32.to_string value in
                raw_input ~a:(a_step (`Step 1.0) :: a) ~input_type:`Number ?value ~name ()
              | Int64 ->
                let value = Option.map Int64.to_string value in
                raw_input ~a:(a_step (`Step 1.0) :: a) ~input_type:`Number ?value ~name ()
              | Float ->
                let value = Option.map string_of_float value in
                raw_input ~a:(a_step `Any :: a) ~input_type:`Number ?value ~name ()
              | Bool ->
                raw_checkbox ~a ?checked:value ~name ~value:"" ()

  and aux_form : type a b . ?is_outmost:bool -> a configs -> b value option -> (a, b) p -> string -> b t -> form_content elt =
    fun ?(is_outmost=false) configs value path name t ->
      let configs =
        if is_outmost then
          Configs.add Root { Config.zero with a = [ a_class [form_outmost_class] ] } configs
        else configs
      in
      match t with
        | Atomic atomic ->
          aux_atomic configs value path name atomic
        | Tuple tuple ->
          aux_form_tuple configs value path name tuple
        | Sum sum ->
          aux_form_sum configs value path name sum
        | Record record ->
          aux_form_record configs value path name record
        | Option t ->
          aux_form_option configs value path name t
        | List t ->
          aux_form_list configs value path name t
        | Array t ->
          aux_form_array configs value path name t
        | Variant tagspecs ->
          aux_form_variant configs value path name tagspecs
        | Function _ ->
          failwith "Generate_form.form: not for functions"
        | Ref _ ->
          failwith "Generate_form.form: not for refs"

  and aux_form_option : type a b . a configs -> b option value option -> (a, b option) p -> string -> b t -> form_content elt =
    let open Html5.F in
    fun configs value path name t ->
      let { value ; a ; label ; annotation ; template } = configs_find_with_value path configs value in
      assert (template = None);
      let annotation =
        flip Option.map annotation @ fun annotation ->
          [ Html5.F.div ~a:[a_class [form_annotation_class]]
              [pcdata annotation] ]
      in
      let selector =
        let checked = Option.map ((<>) None) @ Option.map Value.get value in
        let a = [a_class[option_selector_class]] in
        Html5.D.raw_checkbox ~a ?checked ~name:"" ~value:"" ()
      in
      let content =
        let path = Option_some path in
        let value = Option.bind value Value.put_over_option in
        div ~a:[a_class [option_content_class]]
          [ aux_form configs value path name t ]
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
      let a = a_class [form_class; option_class] :: a in
      Html5.D.div ~a ([ marked selector; content  ] @@ Option.default [] annotation)

  and aux_form_list : type a b . a configs -> b list value option -> (a, b list) p -> name -> b t -> form_content elt =
    fun configs value path name t ->
      let { value ; a ; label ; annotation ; template } = configs_find_with_value path configs value in
      assert (template = None);
      let annotation =
        Option.default [] @
          flip Option.map annotation @ fun annotation ->
            [ Html5.F.div ~a:[a_class [form_annotation_class]]
                [pcdata annotation] ]
      in
      let add =
        Html5.D.Raw.a ~a:[a_class [button_add_class]] [
          span ~a:[a_class [label_class]] [pcdata "add"];
          marker
        ]
      in
      let add_li = Html5.D.li [ add ] in
      let content =
        let items =
          Option.default [] @
            flip Option.map value @ fun list ->
              flip List.mapi (Value.get list) @ fun ix value ->
                let path = List_item (ix, path) in
                let value = Some (Value.kind list value) in
                aux_list_item configs value path ix name t
        in
        Html5.D.ol ~a:[Html5.F.a_start 0] @ items @@ [ add_li ]
      in
      ignore {unit{
        onload_or_now @ fun () ->
          Lwt.async @ fun () ->
            let add = Html5.To_dom.of_element %add in
            let content = Html5.To_dom.of_element %content in
            Lwt_js_events.clicks add @ fun _ _ ->
              let ix = pred @ content ## childNodes ## length in
              let item = !aux_list_item_ref ix %name %(Any_t t) in
              Html5.Manip.appendChild ~before:%add_li %content item;
              reset_required @
                Option.default_delayed (fun () -> Eliom_lib.error_any item "No outmost'") @
                  parent_with_class form_outmost_class content;
              Lwt.return ()
      }};
      let a = a_class [form_class; list_class] :: a in
      Html5.D.div ~a (content :: annotation)

  and aux_form_array : type a b . a configs -> b array value option -> (a, b array) p -> name -> b t -> form_content elt =
    fun configs value path name t ->
      let { value ; a ; label ; annotation ; template } = configs_find_with_value path configs value in
      assert (template = None);
      let content =
        let items =
          Option.default [] @
            flip Option.map value @ fun array ->
              flip List.mapi (Array.to_list @ Value.get array) @ fun ix value ->
                let path = Array_item (ix, path) in
                let value = Some (Value.kind array value) in
                aux_array_item configs value path ix name t
        in
        Html5.D.ol ~a:[Html5.F.a_start 0] items
      in
      let a = a_class [form_class; array_class] :: a in
      Html5.D.div ~a [content]

  type 'a pathed_config =
    | Pathed_config : ('a, 'b) p * 'b config -> 'a pathed_config
    | Pathed_deep_config : ('a, 'b) p * 'b pathed_config list -> 'a pathed_config

  let rec flatten_pathed_config : type a b . (a, b) p -> b pathed_config list -> a configs -> a configs =
    fun p pcs cs ->
      flip2 List.fold_left cs pcs @
        fun cs pc ->
          match pc with
            | Pathed_config (p', c) ->
              Configs.add (compose p' p) c cs
            | Pathed_deep_config (p', pcs) ->
              flatten_pathed_config (compose p' p) pcs cs

  let content : type w a . ?configs:a pathed_config list -> a t ->
                  [ `One of a Eliom_parameter.caml ] Eliom_parameter.param_name -> form_content elt =
    fun ?(configs=[]) t name ->
      let configs = flatten_pathed_config Root configs Configs.zero in
      let name = (Obj.magic name : string) in
      let content = aux_form ~is_outmost:true configs None Root name t in
      ignore {unit{
        onload_or_now @ fun () ->
          let content = Html5.To_dom.of_element %content in
          reset_required content;
          match parent Dom_html.(tagged |- function Form _ -> true | _ -> false) content with
            | None ->
               Firebug.console##log_2 (Js.string "Content not within form; call Eliom_form_generator.init_form on form", content);
               Eliom_content_core.Html5.Custom_data.set_dom content
                 name_any_t_custom_data (%name, %(Deriving_Typerepr.Any_t t))
            | Some form ->
              let form =
                Js.Opt.get (Dom_html.CoerceTo.form form)
                  (fun () -> assert false)
              in
              init_form %name %(Deriving_Typerepr.Any_t t) form
      }};
      content

  module Pathed_config = struct
    let (-->) p = function
      | `Config c -> Pathed_config (p, c)
      | `Tree pcs -> Pathed_deep_config (p, pcs)
    let (/) p1 p2 = compose p2 p1
    let config ?value ?label ?annotation ?(a=[]) ?template () =
      `Config { value ; label ; annotation ; a ; template }
    let tree pcs = `Tree pcs
    let constant x = `Constant x
    let default x = `Default x
    let hidden x = `Hidden x
    include Path
  end

  let rec json_module_of_typerepr : type a . a t -> (module Deriving_Json.Json with type a = a) =
    let for_atomic : type a . a atomic -> (module Deriving_Json.Json with type a = a) = function
      | Unit -> (module Deriving_Json.Json_unit)
      | Int -> (module Deriving_Json.Json_int)
      | Bool -> (module Deriving_Json.Json_bool)
      | String -> (module Deriving_Json.Json_string)
      | Float -> (module Deriving_Json.Json_float)
      | Int32 -> (module Deriving_Json.Json_int32)
      | Int64 -> (module Deriving_Json.Json_int64)
    in
    function
      | Atomic atomic -> for_atomic atomic
      | List t ->
        let module Json = (val json_module_of_typerepr t) in
        (module Deriving_Json.Json_list (Json))
      | Option t ->
        let module Json = (val json_module_of_typerepr t) in
        (module Deriving_Json.Json_option (Json))
      | Array t ->
        let module Json = (val json_module_of_typerepr t) in
        (module Deriving_Json.Json_array (Json))
      | Ref t ->
        let module Json = (val json_module_of_typerepr t) in
        (module Deriving_Json.Json_ref (Json))
      | Tuple ({ components } as tuple) ->
        (module
           Deriving_Json.Defaults
             (struct
               type x = a
               type a = x
               let write buffer value =
                 Buffer.add_string buffer "[0";
                 begin
                   flip List.iter components @ fun (Any_component (Component (t, _) as component)) ->
                     let module Json = (val json_module_of_typerepr t) in
                     Buffer.add_char buffer ',';
                     Json.write buffer @
                       get_tuple_component component value
                 end;
                 Buffer.add_char buffer ']'
               let read buf =
                 let create_tuple_component =
                   fun (type b) (Component ((t : b t), _)) ->
                     let module Json = (val json_module_of_typerepr t) in
                     Deriving_Json_lexer.read_comma buf;
                     Json.read buf
                 in
                 Deriving_Json_lexer.read_lbracket buf;
                 ignore (Deriving_Json_lexer.read_tag_1 0 buf);
                 let res = create_tuple tuple { create_tuple_component } in
                 ignore @ Deriving_Json_lexer.read_rbracket buf;
                 res
              end))
      | Sum ({ summands } as sum) ->
        (module
           Deriving_Json.Defaults
             (struct
               type x = a
               type a = x
               let write buffer value =
                 let _, Any_case_value (summand, value) = get_sum_case sum value in
                 match summand with
                   | Summand_nullary nullary ->
                     let ix = (nullary : _ nullary :> int) in
                     Printf.bprintf buffer "%d" ix
                   | Summand_unary unary ->
                     let ix, t = (unary : (_, _) unary :> _ * _) in
                     let module Json = (val json_module_of_typerepr t) in
                     Printf.bprintf buffer "[%d," ix;
                     Json.write buffer value;
                     Buffer.add_char buffer ']'
                   | Summand_nary nary ->
                     let ix, { components } = (nary : (_, _) nary :> _ * _) in
                     Printf.bprintf buffer "[%d" ix;
                     begin
                       flip List.iter components @ fun (Any_component component) ->
                         let Component (t, _) = component in
                         let module Json = (val json_module_of_typerepr t) in
                         Buffer.add_char buffer ',';
                         Json.write buffer @ get_tuple_component component value
                     end;
                     Buffer.add_char buffer ']'
               let read buf =
                 let is_nullary = function
                   | _, Any_summand (Summand_nullary _) -> true
                   | _ -> false
                 in
                 match Deriving_Json_lexer.read_case buf with
                   | `Cst ix ->
                     let _, Any_summand summand = flip List.nth ix @ List.filter is_nullary summands in
                     begin
                       match summand with
                         | Summand_nullary _ as summand ->
                           create_sum_case summand ()
                         | _ -> assert false
                     end
                   | `NCst ix ->
                     begin
                       let _, Any_summand summand =
                         flip List.nth ix @
                           List.filter (not -| is_nullary) summands
                       in
                       match summand with
                         | Summand_unary unary ->
                           let _, t = (unary : (_, _) unary :> _ * _) in
                           let module Json = (val json_module_of_typerepr t) in
                           Deriving_Json_lexer.read_comma buf;
                           let value = Json.read buf in
                           ignore @ Deriving_Json_lexer.read_rbracket buf;
                           create_sum_case summand value
                         | Summand_nary nary ->
                           let tuple =
                             let create_tuple_component (type b) (Component ((t : b t), _)) =
                               let module Json = (val json_module_of_typerepr t) in
                               Deriving_Json_lexer.read_comma buf;
                               Json.read buf
                             in
                             let _, t = (nary : (_, _) nary :> _ * _) in
                             create_tuple t { create_tuple_component }
                           in
                           let res = create_sum_case summand tuple in
                           ignore @ Deriving_Json_lexer.read_rbracket buf;
                           res
                         | Summand_nullary _ -> assert false
                     end
              end))
      | Variant ({ tagspecs } as variant) ->
        (module
           Deriving_Json.Defaults (* FIXME Implement for Deriving_Json.Defaults' *)
             (struct
               type x = a
               type a = x
               let write buffer value =
                 let _, Any_variant_value (tagspec, value) = get_variant_case variant value in
                 match tagspec with
                   | Tag_nullary nullary ->
                     let ix = (nullary : _ nullary :> int) in
                     Printf.bprintf buffer "%d" ix
                   | Tag_unary unary ->
                     let ix, t = (unary : (_, _) unary :> _ * _) in
                     let module Json = (val json_module_of_typerepr t) in
                     Printf.bprintf buffer "[0,%d," ix;
                     Json.write buffer value;
                     Buffer.add_char buffer ']'
                   | Tag_nary nary ->
                     let ix, { components } = (nary : (_, _) nary :> _ * _) in
                     Printf.bprintf buffer "[%d" ix;
                     begin
                       flip List.iter components @ fun (Any_component component) ->
                         let Component (t, _) = component in
                         let module Json = (val json_module_of_typerepr t) in
                         Buffer.add_char buffer ',';
                         Json.write buffer @ get_tuple_component component value
                     end;
                     Buffer.add_char buffer ']'
               let read buf =
                 let find_tagspec ix =
                   flip List.find tagspecs @ fun (_, Any_tagspec tagspec) ->
                     ix =
                       match tagspec with
                         | Tag_nullary nullary ->
                           (nullary : _ nullary :> int)
                         | Tag_unary unary ->
                           fst (unary : (_,_) unary :> _ * _)
                         | Tag_nary nary ->
                           fst (nary : (_,_) nary :> _ * _)
                 in
                 match Deriving_Json_lexer.read_case buf with
                   | `Cst ix ->
                     let _, Any_tagspec tagspec = find_tagspec ix in
                     begin
                       match tagspec with
                         | Tag_nullary _ as tag ->
                           create_variant_case tag ()
                         | _ -> assert false
                     end
                   | `NCst 0 ->
                     begin
                       Deriving_Json_lexer.read_comma buf;
                       let ix = Deriving_Json.Json_int.read buf in
                       let _, Any_tagspec tagspec = find_tagspec ix in
                       match tagspec with
                         | Tag_unary unary ->
                           let _, t = (unary : (_, _) unary :> _ * _) in
                           let module Json = (val json_module_of_typerepr t) in
                           Deriving_Json_lexer.read_comma buf;
                           let value = Json.read buf in
                           ignore @ Deriving_Json_lexer.read_rbracket buf;
                           create_variant_case tagspec value
                         | Tag_nary nary ->
                           let tuple =
                             let create_tuple_component (type b) (Component ((t : b t), _)) =
                               let module Json = (val json_module_of_typerepr t) in
                               Deriving_Json_lexer.read_comma buf;
                               Json.read buf
                             in
                             let _, t = (nary : (_, _) nary :> _ * _) in
                             create_tuple t { create_tuple_component }
                           in
                           let res = create_variant_case tagspec tuple in
                           ignore @ Deriving_Json_lexer.read_rbracket buf;
                           res
                         | Tag_nullary _ -> assert false
                     end
                   | _ -> failwith "Json_Json: Unexpected constructor"
              end))
      | Record ({ fields } as record) ->
        (module
           Deriving_Json.Defaults
             (struct
               type x = a
               type a = x
               let write buffer value =
                 Buffer.add_string buffer "[0";
                 begin
                   flip List.iter fields @ fun (_, Any_field field) ->
                     let Field (_, t) = field in
                     let module Json = (val json_module_of_typerepr t) in
                     Buffer.add_char buffer ',';
                     Json.write buffer @ get_record_field field value
                 end;
                 Buffer.add_string buffer "]"
               let read buf =
                 let create_record_field (type  b) _ (Field (_, (t : b t))) =
                   let module Json = (val json_module_of_typerepr t) in
                   Deriving_Json_lexer.read_comma buf;
                   Json.read buf
                 in
                 Deriving_Json_lexer.read_lbracket buf;
                 ignore (Deriving_Json_lexer.read_tag_2 0 254 buf);
                 let res = create_record record { create_record_field } in
                 Deriving_Json_lexer.read_rbracket buf;
                 res
              end))
      | Function _ ->
        failwith "Eliom_form_generator.json_of_typerepr: function"

  let json_of_typerepr : type a . a t -> a Deriving_Json.t =
    fun t ->
      let module Json = (val json_module_of_typerepr t) in
      Json.t
}}

{client{
  let () =
    aux_list_item_ref :=
      fun ix name (Any_t t) ->
        aux_list_item Configs.zero None Root ix name t

  let init_form form =
    Eliom_lib.debug "Eliom_form_generator.init_form";
    let name, any_t =
      let content =
        Js.Opt.get (form ## querySelector (js_string ".%s" form_outmost_class)) @
          fun () ->
            Eliom_lib.error_any content "Eliom_form_generator.init_form: No outmost content"
      in
      try
        Eliom_content.Html5.Custom_data.get_dom content
          name_any_t_custom_data
      with Not_found ->
        Eliom_lib.error_any content "Eliom_form_generator.init_form: Already called"
    in
    Eliom_lib.debug "Eliom_form_generator.init_form: found name %S" name;
    init_form name any_t form
}}
