

{shared{
  open Deriving_Form_base
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
  let rearrange_input_names prefix ?new_item list_node =
    let set_input_names new_index node =
      Firebug.console ## log_2 (Js.string "set_input_names", node);
      let const = prefix_concat prefix list_combined_suffix in
      (* pattern of prefix:
         eliom_prefix ^_0 const ^_1 between ^_2 [N ^_3 ] ^ rest *)
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
      let force_read_write_name : < name : Js.js_string Js.t Js.readonly_prop ; .. > Js.t -> < name : Js.js_string Js.t Js.prop > Js.t =
        Obj.magic (* Force name to be writeable - it's readonly in js_of_ocaml due tue IE8 *)
      in
      let opt = Dom_html.CoerceTo.input node in
      if Js.Opt.test opt then
        Js.Opt.map opt force_read_write_name
      else
        Js.Opt.map (Dom_html.CoerceTo.select node) force_read_write_name
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
            set_required_for_outmost ~is_outmost [list] @ [ add_a ]

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
}}