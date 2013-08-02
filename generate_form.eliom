

{shared{

  open Html5_types
  open Eliom_content
  type 'a elt = 'a Eliom_content.Html5.elt

  external (@) : ('a -> 'b) -> 'a -> 'b = "%apply"
  external identity : 'a -> 'a = "%identity"
  let (@@) = List.append
  let flip f x y = f y x
  let flip2 f x y z = f z x y
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
  end
}}

{shared{

  let form_outmost_class = "eliom-form-outmost"
  let form_class = "__eliom_form__"
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
  let label_class = "label"
  let marker_class = "__eliom_form_input_marker__"
  let selector_snippet = "selector"
  let content_snippet = "content"
  let marker = Eliom_content.Html5.F.(span ~a:[a_class [marker_class]] [])
}}


{client{

  let js_string fmt = Printf.ksprintf Js.string fmt

  let set_required node is_required =
    let open Dom_html in
    match tagged node with
      | Input input -> input ## required <- Js.bool is_required
      | Select select -> select ## required <- Js.bool is_required
      | Textarea textarea -> textarea ## required <- Js.bool is_required
      | A _|Area _|Base _|Blockquote _|Body _|Br _|Button _|Canvas _|Caption _|
        Col _|Colgroup _|Del _|Div _|Dl _|Fieldset _|Form _|Frameset _|Frame _|
        H1 _|H2 _|H3 _|H4 _|H5 _|H6 _|Head _|Hr _|Html _|Iframe _|Img _|Ins _|
        Label _|Legend _|Li _|Link _|Map _|Meta _|Object _|Ol _|Optgroup _|Option _|
        P _|Param _|Pre _|Q _|Script _|Style _|Table _|Tbody _|Td _|
        Tfoot _|Th _|Thead _|Title _|Tr _|Ul _|Other _ -> ()

  let has_class node class_ =
    let node =
      Js.Opt.get (Dom_html.CoerceTo.element (node :> Dom.node Js.t))
        (fun () -> failwith "Not a html element")
    in
    Js.to_bool @ node ## classList ## contains (Js.string class_)

  let rec parent_with_class ?(inclusive=false) ?(before=fun _ -> false) class_ node =
    if before (node :> Dom.node Js.t) then
      None
    else
      if inclusive && has_class node class_ then
        Some node
      else
        Js.Opt.case (node ## parentNode) (fun () -> None) @
          fun parent ->
            Js.Opt.case (Dom_html.CoerceTo.element parent)
              (fun () -> None)
              (parent_with_class ~inclusive:true ~before class_)

  let parent_with_class ?inclusive ?before class_ node =
    parent_with_class ?inclusive ?before class_ (node :> Dom_html.element Js.t)

  let ancestors_with_class ?before class_ node =
    let rec aux acc node =
      match parent_with_class ?before class_ node with
        | None -> acc
        | Some parent ->
          aux (parent :: acc) parent
    in
    aux [] node

  let rec path =
    fun from to_ ->
      if from = to_ then
        []
      else
        Js.Opt.case (from ## parentNode)
          (fun () -> Eliom_lib.error "Generate_form.path: to_ not in ancestors")
          (fun parent ->
            from :: path parent to_)
  let path from to_ = path (from :> Dom.node Js.t) (to_ :> Dom.node Js.t)

  let categorize_form node =
    if not @ has_class node form_class then
      failwith "Generate_form.categorize_form";
    let test (label, _) = has_class node label in
    snd @ List.find test [
      option_class , `Option ;
      sum_class, `Sum ;
      tuple_class, `Tuple ;
      record_class, `Record  ;
    ]

  let querySelector ?(not_between=fun _ -> false) node fmt =
    flip Printf.ksprintf fmt @ fun selector ->
      let node = (node :> Dom_html.element Js.t) in
      let nodes = Dom.list_of_nodeList @ node ## querySelectorAll (Js.string selector) in
      Option.from_list  @
        flip List.filter nodes @ fun node' ->
          not @ List.exists not_between @ path node' node

  let is_required_local node =
    let form =
      Option.get' (fun () -> Eliom_lib.error_any node "Generate_form.is_required: Not within form") @
        parent_with_class form_class node
    in
    match categorize_form form with
      | `Tuple -> true
      | `Record -> true
      | `Option ->
        if has_class node option_selector_class then
          false
        else
          let selector =
            let input =
              Option.get' (fun () -> Eliom_lib.error_any form "Generate_form.is_required_local: Option without selector") @
                querySelector ~not_between:(flip has_class form_class) form ".%s" option_selector_class
            in
            Js.Opt.get (Dom_html.CoerceTo.input input)
              (fun () -> Eliom_lib.error_any input "Generate_form.is_required_local: Option selector not input")
          in
          Js.to_bool @ selector ## checked
      | `Sum ->
        if has_class node sum_selector_class then
          true
        else
          let selector =
            let select =
              Option.get' (fun () -> Eliom_lib.error_any form "Generate_form.is_required_local: Sum without selector") @
                querySelector ~not_between:(flip has_class form_class) form ".%s" sum_selector_class
            in
            Js.Opt.get (Dom_html.CoerceTo.select select)
              (fun () -> Eliom_lib.error_any select "Generate_form.is_required_local: Sum selector not input")
          in
          let case = Js.to_string @ selector ## value in
          let case_content =
            Option.get' (fun () -> Eliom_lib.error_any node "Generate_form.is_required_local: input not within case marker %s" sum_case_class) @
              parent_with_class ~before:((==) (form :> Dom.node Js.t)) sum_case_class node
          in
          has_class case_content @ sum_case_marker_class case

  let rec is_required node =
    is_required_local node &&
      Option.get true @
        Option.map is_required @
          parent_with_class ~before:(flip has_class form_outmost_class)
            form_class node

  let reset_required (outmost : #Dom_html.element Js.t) =
    flip List.iter (Dom.list_of_nodeList @ outmost ## querySelectorAll (js_string "input,select,textarea")) @ fun input ->
      set_required input @ is_required input

}}
{shared{

  type prefix = string list
  let extend prefix suffix = prefix @@ [ suffix ]
  let extend_special prefix suffix = prefix @@ [ "."^suffix ]
  let prefix_to_string = String.concat "-"

  open Html5.F

  let rec aux_form_tuple : 'a . ?local_a:_ -> ?value:'a -> prefix -> _ -> form_content elt list =
    fun ?(local_a=[]) ?value prefix (`Tuple exprs) ->
      [ div ~a:(a_class [ form_class ; tuple_class ] :: local_a) @
          List.concat @
            flip List.mapi exprs @ fun ix expr ->
              let value = Option.map (Deriving_Typerepr.get_component ix) value in
              let prefix = extend prefix @ string_of_int ix in
              aux_form ?value prefix expr]

  and aux_form_sum  : 'a . ?local_a:_ -> ?value:'a -> prefix -> _ -> form_content elt list =
    fun ?(local_a=[]) ?value prefix (`Sum summands) ->
      let sub_value = Option.map (Deriving_Typerepr.get_case (`Sum summands)) value in
      let selector =
        let a = [ a_class [ sum_selector_class ] ] in
        let name = prefix_to_string @ extend_special prefix selector_snippet in
        let null = Html5.D.Option ([], "", Some (pcdata "- select -"), value = None) in
        let summands =
          flip List.map summands @ fun (name, _) ->
            let selected =
              Option.get false @
                flip Option.map sub_value @ fun ((name',_),_) -> name = name'
            in
            Html5.D.Option ([], name, Some (pcdata name), selected)
        in
        Html5.D.raw_select ~a ~name null summands
      in
      let content =
        div ~a:[a_class [sum_content_class]] @
          flip List.map summands @ fun (name, exprs) ->
            let value = Option.map snd sub_value in
            div ~a:[a_class [sum_case_class; sum_case_marker_class name]] @
              aux_form_tuple ?value (extend prefix name) (`Tuple exprs)
      in
      ignore {unit{
        Eliom_client.onload @ fun () ->
          Lwt.async @ fun () ->
            let selector = Html5.To_dom.of_select %selector in
            Lwt_js_events.changes selector @ fun _ _ ->
              Lwt.return @
                match parent_with_class form_outmost_class selector with
                  | Some outmost -> reset_required outmost
                  | None -> failwith "Generate_form: no outmost"
      }};
      [ div ~a:(a_class [form_class; sum_class] :: local_a)
          [span [selector; marker]; content ] ]

  and aux_form_record : 'a . ?local_a:_ -> ?value:'a -> prefix -> _ -> form_content elt list =
    fun ?(local_a=[]) ?value prefix (`Record fields) ->
      let rows =
        flip List.map fields @ fun ((name, (params, expr)) as field) ->
          if params <> [] then
            failwith ("Generate_form.generate_form: params for field "^name);
          let content =
            let value = Option.map (Deriving_Typerepr.get_field (`Record fields) field) value in
            aux_form ?value (extend prefix name) expr
          in
          tr [td ~a:[a_class [label_class]] [pcdata name]; td content]
      in
      [ table ~a:(a_class[form_class; record_class] :: local_a)
          (List.hd rows) (List.tl rows) ]

  and aux_form_constr :  'a . ?local_a:_ -> ?value:'a -> prefix -> _ -> form_content elt list =
    fun ?(local_a=[])
        ?value prefix (`Constr (name, exprs)) ->
      let a = a_class[form_class; atomic_class] :: (local_a :> Html5_types.input_attrib attrib list) in
      match name, exprs with
        | [ "unit" ], [] -> []
        | [ "string" ], [] ->
          let value = flip Option.map value @ fun value -> (Obj.magic value : string) in
          [ Html5.F.raw_input ~a ~input_type:`Text ?value ~name:(prefix_to_string prefix) (); marker ]
        | [ "int" ], [] ->
          let value = flip Option.map value @ fun value -> (Obj.magic value : int) in
          let value = Option.map string_of_int value in
          [ Html5.F.raw_input ~a ~input_type:`Number ?value ~name:(prefix_to_string prefix) (); marker ]
        | [ "int32" ], [] ->
          let value = flip Option.map value @ fun value -> (Obj.magic value : int32) in
          let value = Option.map Int32.to_string value in
          [ Html5.F.raw_input ~a ~input_type:`Number ?value ~name:(prefix_to_string prefix) (); marker ]
        | [ "int64" ], [] ->
          let value = flip Option.map value @ fun value -> (Obj.magic value : int64) in
          let value = Option.map Int64.to_string value in
          [ Html5.F.raw_input ~a ~input_type:`Number ?value ~name:(prefix_to_string prefix) (); marker ]
        | [ "float" ], [] ->
          let value = flip Option.map value @ fun value -> (Obj.magic value : float) in
          let value = Option.map string_of_float value in
          [ Html5.F.raw_input ~a ~input_type:`Number ?value ~name:(prefix_to_string prefix) (); marker ]
        | [ "bool" ], [] ->
          let value =flip Option.map value @ fun value ->  (Obj.magic value : bool) in
          let checked = flip Option.map value @ fun value -> (Obj.magic value : bool) in
          [ Html5.F.raw_checkbox ~a ?checked ~name:(prefix_to_string prefix) ~value:"" (); marker ]
        | [ "option" ], [ expr ] ->
          let value = flip Option.map value @ fun value -> (Obj.magic value : _ option) in
          let selector =
            let name = prefix_to_string @ extend_special prefix selector_snippet in
            let checked = Option.map ((<>) None) value in
            let a = [a_class[option_selector_class]] in
            Html5.D.raw_checkbox ~a ?checked ~name ~value:"" ()
          in
          let content =
            let a = [a_class[option_content_class]] in
            let value = Option.bind value identity in
            div ~a @ aux_form ?value (extend_special prefix content_snippet) expr
          in
          ignore {unit{
            Eliom_client.onload @ fun () ->
              Lwt.async @ fun () ->
                let selector = Html5.To_dom.of_input %selector in
                Lwt_js_events.changes selector @ fun _ _ ->
                  Lwt.return @
                    match parent_with_class form_outmost_class selector with
                      | Some outmost -> reset_required outmost
                      | None -> failwith "Generate_form: no outmost"
          }};
          let a = a_class[form_class; option_class] :: (local_a :> Html5_types.div_attrib attrib list) in
          [ Html5.F.div ~a [ span [selector; marker]; content] ]
        | qname, exprs -> Printf.ksprintf failwith
          "Generate_form.form: Unknown constructor %s (with %i parameters)"
          (String.concat "." qname) (List.length exprs)


  and aux_form ?(is_outmost=false) ?value prefix expr =
    let local_a =
      if is_outmost then
        Some [ a_class [ form_outmost_class ] ]
      else None
    in
    match expr with
      | `Tuple _ as repr ->
        aux_form_tuple ?local_a ?value prefix repr
      | `Sum _ as repr ->
        aux_form_sum ?local_a ?value prefix repr
      | `Record _ as repr ->
        aux_form_record ?local_a ?value prefix repr
      | `Constr _ as repr ->
        aux_form_constr ?local_a ?value prefix repr
      | `Label _ | `Function _ ->
        failwith "Generate_form.form: not for functions"

  let form : 'a Deriving_Typerepr.t -> 'a Deriving_Json.t -> ?submit:button_content elt list -> ?value:'a -> [ `One of 'a Eliom_parameter.caml ] Eliom_parameter.param_name -> form_content elt list =
    fun expr _ ?submit ?value name ->
      let prefix = [ (Obj.magic name : string) ] in
      let content = aux_form ~is_outmost:true ?value prefix expr in
      let button =
        Option.get [] @ flip Option.map submit @ fun submit ->
          [ Html5.D.button ~button_type:`Button submit ]
      in
      let res = Html5.D.div (content @@ button) in
      ignore {unit{
        Eliom_client.onload @ fun () ->
          let res = Html5.To_dom.of_element %res in
          let outmost =
            Js.Opt.get (res ## querySelector (js_string ".%s" form_outmost_class))
              (fun () -> Eliom_lib.error_any res "Generate_form: no outmost")
          in
          reset_required outmost
      }};
      [ res ]
}}


(* let form = *)
(*   Eliom_content.Html5.F.get_form ~service @ *)
(*     Generate_form.form Typerepr.t<mytype> Json.t<mytype> *)
