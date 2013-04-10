
open Pa_deriving_common

module Description = struct
  let classname = "Form"
  let runtimename = "Deriving_Form"
  let default_module = None
  let alpha = None
  let allow_private = false
  let predefs = []
  let depends = []
end

module Builder (Loc : Defs.Loc) = struct

  module Generator = Base.Generator(Loc)(Description)

  module Helpers = struct
    include Base.AstHelpers(Loc)
    module Untranslate' = struct
      include Untranslate
      let expr =
        let open Loc in
        let open Camlp4.PreCast in
        let rec expr : Type.expr -> Ast.ctyp = function
            `Param p -> param p
          | `Function (f, t) -> <:ctyp< $expr f$ -> $expr t$ >>
          | `Tuple [t] -> expr t
          | `Tuple ts -> Ast.TyTup (_loc, Ast.tySta_of_list (List.map expr ts))
          | `Constr (tcon, args) -> app (Ast.TyId (_loc, qname tcon)) args
          | `Label (`NonOptional, name, type1, type2) ->
            <:ctyp< $name$:$expr type1$ -> $expr type2$ >>
          | `Label (`Optional, name, type1, type2) ->
            <:ctyp< ? $name$:$expr type1$ -> $expr type2$ >>
          | _ -> assert false
        and app f = function
          | [] -> f
          | [x] -> <:ctyp< $f$ $expr x$ >>
          | x::xs -> app (<:ctyp< $f$ $expr x$ >>) xs
        in expr
    end
  end

  open Loc
  open Camlp4.PreCast

  let fold_right1 : ('a -> 'a -> 'a) -> 'a list -> 'a =
    fun f li ->
      match List.rev li with
      | hd :: tl ->
        List.fold_right f (List.rev tl) hd
      | [] -> failwith "fold_right1"

  let ident_of_qname qname =
    fold_right1
      (fun id1 id2 -> <:ident< $id1$ . $id2$ >>)
      (List.map
         (fun name -> <:ident< $uid:name$ >>)
         qname)

  let rec map_last f = function
    | [] -> []
    | [x] -> [f x]
    | hd :: tl -> hd :: map_last f tl

  let rec append x = function
    | [] -> [x]
    | hd :: tl -> hd :: append x tl

  let form_module_name = Printf.sprintf "Form_%s"
  let component_module_name = Printf.sprintf "Component_%s"

  let get_simple_type = function
    | `Constr (type_qname, type_params) ->
      let type_params =
        List.map
          (function
          | `Constr (type_qname, []) -> type_qname
          | _ -> Base.fatal_error _loc "only simple type constructors as type argument allowed")
          type_params
      in
      type_qname, type_params
    | _ -> Base.fatal_error _loc "only simple types allowed"

  let add_prefix prefix qname = prefix :: qname

  let type_expr_from_qname qname = `Constr (List.rev qname, [])

  let tuple_expr exprs =
    fold_right1
      (fun expr sofar ->
        <:expr< ( $expr$, $sofar$ ) >>)
      exprs

  let tuple_pattern =
    fold_right1
      (fun t sofar ->
        <:patt< ( $t$, $sofar$ ) >>)

  let tuple_type =
    fold_right1
      (fun t sofar ->
        <:ctyp< ( $t$ * $sofar$ ) >>)

  let match_case pattern expr =
    Ast.McArr (_loc, pattern, Ast.ExNil _loc, expr)

  let variant_pattern ?(component_pattern= <:patt< _ >> ) name type_opt =
    if type_opt = None then
      <:patt< $uid:name$ >>
    else
      <:patt< $uid:name$ $component_pattern$ >>

  let component_module_decls  =
    List.map2
      (fun component_name opt_type ->
        let type_qname, type_params =
          match opt_type with
            | Some x -> x
            | None -> ["unit"], []
        in
        let module_qname = map_last form_module_name type_qname in
        let me : Ast.module_expr =
          match type_params with
          | [] ->
            <:module_expr< $id:ident_of_qname module_qname$ >>
          | [type_param_qname] ->
            let make_module_qname = append "Make" module_qname in
            let options_module_qname =
              append "Options"
                (map_last form_module_name type_param_qname)
            in
            <:module_expr<
              $id:ident_of_qname make_module_qname$
                ($id:ident_of_qname options_module_qname$)
            >>
          | _ -> failwith "component_module_decls"
        in
        <:str_item< module $uid:component_module_name component_name$ = $mexp:me$ >>)

  let generate : Type.decl list -> Ast.str_item =
    let (%) f g = fun x -> f (g x) in
    let for_decl : Type.decl -> Ast.str_item  = function
      | type_name, [], `Fresh (None, repr, _), [], _ ->
        let component_names, component_types =
          let fields =
            match repr with
              | Type.Record record_fields ->
                List.map
                  (fun (name, (poly_args, expr), _) ->
                    if poly_args <> [] then
                      Base.fatal_error _loc "no polymorphic arguments allowed";
                    name, Some (get_simple_type expr))
                  record_fields
              | Type.Sum summands ->
                List.map
                  (function
                    | name, [] -> name, None
                    | name, [expr] -> name, Some (get_simple_type expr)
                    | _ -> Base.fatal_error _loc "Only nullary or unary variants allowed")
                  summands
          in
          List.split fields
        in
        let component_module_decls = component_module_decls component_names component_types in
        let component_module_names =
          List.map
            (function field_name ->
              [component_module_name field_name])
            component_names
        in
        let param_names_ctyp =
          let basic =
            tuple_type
              (List.map (Helpers.Untranslate'.expr % type_expr_from_qname % add_prefix "param_names")
                 component_module_names)
          in
          match repr with
          | Type.Record _ -> basic
          | Type.Sum _ ->
            <:ctyp< [`Radio of string] Eliom_parameter.param_name * $basic$ >>
        in
        let deep_config_ctyp =
          let optional typ = <:ctyp< $typ$ option >> in
          tuple_type
            (List.map (optional % Helpers.Untranslate'.expr %
                         type_expr_from_qname % add_prefix "config")
               component_module_names)
        in
        let default_deep_config_expr =
          tuple_expr
            (List.map (fun _ -> <:expr< None >>)
               component_module_names)
        in
        let repr_ctyp =
          let tuple =
            List.map (type_expr_from_qname % add_prefix "repr")
              component_module_names
          in
          match repr with
            | Type.Record _ -> tuple_type (List.map Helpers.Untranslate'.expr tuple)
            | Type.Sum _ ->
              let option_tuple =
                let optional typ = <:ctyp< $typ$ option >> in
                tuple_type
                  (List.map (optional % Helpers.Untranslate'.expr) tuple)
              in
              <:ctyp< string option * $option_tuple$ >>
        in
        let component_tuple_expr =
          tuple_expr
            (List.map
               (fun component_name ->
                 <:expr< $lid:String.uncapitalize component_name$ >>)
               component_names)
        in
        let to_tuple_expr =
          fold_right1
            (fun t sofar ->
              <:expr< $t$, $sofar$ >>)
            (List.map
               (fun field_name ->
                 <:expr< $uid:component_module_name field_name$ . to_repr $lid:field_name$ >>)
               component_names)
        in
        let pattern_from_name name = <:patt< $lid:String.uncapitalize name$ >> in
        let from_tuple_bindings =
          List.map
            (fun field_name ->
              field_name, <:expr< $uid:component_module_name field_name$ . of_repr $lid:field_name$ >>)
            component_names
        in
        let component_name_strings =
          List.map
            (fun component_name -> <:expr< $str:component_name$ >>)
            component_names
        in
        let opt_component_configs_fun_type : Type.expr =
          List.fold_right
            (fun component_name sofar ->
              `Label
                (`Optional,
                 String.uncapitalize component_name,
                 `Constr ([component_module_name component_name; "config"], []),
                 sofar))
            component_names
            (`Function
                (`Constr (["unit"], []),
                 `Function (`Param ("arg", None), `Param ("res", None))))
        in
        let opt_component_configs_fun =
          List.fold_right
            (fun component_name sofar ->
              <:expr< fun ? $lid:String.uncapitalize component_name$ -> $sofar$ >>)
            component_names
            (<:expr< fun () arg -> k $component_tuple_expr$ arg >>)
        in
        let params_type_expr =
          let product =
            fold_right1
              (fun component_type sofar ->
                <:expr< Eliom_parameter.prod $component_type$ $sofar$ >>)
          in
          match repr with
            | Type.Record _ ->
              let params_types =
                List.map
                  (fun component_name ->
                    <:expr<
                      $uid:component_module_name component_name$ . params_type
                        (prefix ^ "_" ^ $str:component_name$ )
                     >>)
                   component_names
              in
              <:expr< fun prefix -> $product params_types$ >>
            | Type.Sum _ ->
              let optional_params_types =
                List.map
                  (fun component_name ->
                    <:expr<
                      Eliom_parameter.opt
                        ($uid:component_module_name component_name$ . params_type
                          (prefix ^ "_" ^ $str:component_name$))
                     >>)
                   component_names
              in
              <:expr<
                fun prefix ->
                  Eliom_parameter.prod
                    (Eliom_parameter.radio Eliom_parameter.string (prefix^"_constructor"))
                    $product optional_params_types$
              >>
        in
        let component_tuple_pattern =
          tuple_pattern (List.map pattern_from_name component_names)
        in
        let fields_expr =
          let project = Printf.sprintf "project_%s" in
          let projection_pel =
            List.map
              (fun field_name ->
                <:patt< $lid:project field_name$ >>,
                <:expr< fun $component_tuple_pattern$ -> $lid:String.uncapitalize field_name$ >>)
              component_names
          in
          let project_default_expr component_name =
            match repr with
              | Type.Record record_fields ->
                <:expr<
                  fun $Helpers.record_pattern record_fields$ ->
                    Some ($lid:component_name$)
                >>
              | Type.Sum summands ->
                let match_cases =
                  List.map2
                    (fun variant_name variant_type_opt ->
                      let binder, expr =
                        if variant_name = component_name then
                          if variant_type_opt = None then
                            <:patt< _ >>, <:expr< Some () >>
                          else
                            <:patt< component >>, <:expr< Some component >>
                        else
                          <:patt< _ >>, <:expr< None >>
                      in
                      let pattern =
                        variant_pattern ~component_pattern:binder
                          variant_name variant_type_opt
                      in
                      match_case pattern expr)
                    component_names component_types
                in
                <:expr<
                  function $Ast.mcOr_of_list match_cases$
                >>
          in
          let project_param_names_expr component_name =
            match repr with
              | Type.Record _ ->
                <:expr< $lid:project component_name$ >>
              | Type.Sum _ ->
                <:expr< fun param_names -> $lid:project component_name$ (snd param_names) >>
          in
          let component_module_decls =
            let component_module component_name =
              let sum_only =
                match repr with
                  | Type.Record _ -> <:str_item< >>
                  | Type.Sum _ ->
                    let is_constructor_expr =
                      let match_cases =
                        List.map2
                          (fun variant_name variant_type_opt ->
                            let pattern = variant_pattern variant_name variant_type_opt in
                            let expr =
                              if variant_name = component_name then
                                <:expr< true >>
                              else
                                <:expr< false >>
                            in
                            match_case pattern expr)
                          component_names component_types
                      in
                      <:expr< function $Ast.mcOr_of_list match_cases$ >>
                    in
                    <:str_item<
                      let project_selector_param_name = fst
                      let is_constructor = $is_constructor_expr$
                    >>
              in
              <:module_expr<
                struct
                  type enclosing_a = a
                  type enclosing_param_names = param_names
                  type enclosing_deep_config = deep_config
                  let project_default = $project_default_expr component_name$
                  let project_param_names = $project_param_names_expr component_name$
                  let project_config = $lid:project component_name$
                  ;; $sum_only$
                  include $uid:component_module_name component_name$
                end
              >>
            in
            let component_module_list =
              Helpers.expr_list
                (List.map
                   (fun component_name ->
                     <:expr< (module $uid:component_module_name component_name$) >>)
                   component_names)
            in
            List.fold_right
              (fun component_name sofar ->
                <:expr<
                  let module $uid:component_module_name component_name$ =
                             $component_module component_name$
                  in $sofar$ >>)
              component_names
              component_module_list
          in
          <:expr<
            let $Ast.binding_of_pel projection_pel$ in
            $component_module_decls$
          >>
        in
        let to_repr_expr =
          match repr with
          | Type.Record record_fields ->
              <:expr<
                fun $Helpers.record_pattern record_fields$ ->
                  $to_tuple_expr$
              >>
          | Type.Sum summands ->
            let match_cases =
              List.map2
                (fun variant_name variant_type_opt ->
                  let component_pattern, component_expr =
                    if variant_type_opt = None then
                      <:patt< _ >>, <:expr< () >>
                    else
                      <:patt< component >>, <:expr< component >>
                  in
                  let pattern =
                    variant_pattern ~component_pattern
                      variant_name variant_type_opt
                  in
                  let expr =
                    let data =
                      tuple_expr
                        (List.map
                           (fun variant_name' ->
                             if variant_name = variant_name' then
                               <:expr<
                                 Some ($uid:component_module_name variant_name$ .
                                         to_repr $component_expr$)
                               >>
                             else
                               <:expr< None >>)
                           component_names)
                    in
                    <:expr< Some $str:variant_name$, $data$ >>
                  in
                  match_case pattern expr)
                component_names component_types
            in
            <:expr< function $Ast.mcOr_of_list match_cases$ >>
        in
        let of_repr_expr =
          match repr with
            | Type.Record record_fields ->
              <:expr<
                fun $pat:component_tuple_pattern$ ->
                  $Helpers.record_expr from_tuple_bindings$
              >>
            | Type.Sum summands ->
              let match_cases =
                List.map2
                  (fun variant_name variant_type_opt ->
                    let component_pattern, component_expr =
                      if variant_type_opt = None then
                        <:patt< _ >>, <:expr< () >>
                      else
                        <:patt< component >>, <:expr< component >>
                    in
                    let data =
                      List.map
                        (fun variant_name' ->
                          if variant_name = variant_name' then
                            <:patt< Some $component_pattern$ >>
                          else
                            <:patt< _ >>)
                        component_names
                    in
                    let pattern =
                      <:patt< Some $str:variant_name$, $tuple_pattern data$ >>
                    in
                    let expr =
                      if variant_type_opt = None then
                        <:expr< $uid:variant_name$ >>
                      else
                        <:expr<
                          $uid:variant_name$
                            ($uid:component_module_name variant_name$ . of_repr $component_expr$)
                        >>
                    in
                    match_case pattern expr)
                  component_names component_types
              in
              let failure =
                let message = Printf.sprintf "%s: of_repr" (form_module_name type_name) in
                match_case ( <:patt< _ >> ) ( <:expr< failwith $str:message$ >> )
              in
              let match_cases = match_cases @ [ failure ] in
              <:expr< function $Ast.mcOr_of_list match_cases$ >>
        in
        let make_module_name, components_list_name, component_type_name =
          match repr with
            | Type.Record _ -> "Make_record", "fields", "field"
            | Type.Sum _ -> "Make_sum", "variants", "variant"
        in
        <:str_item<
          module $uid:form_module_name type_name$ = struct
            open Deriving_Form
            module Options = struct
              type a = $lid:type_name$
              ;; $Ast.stSem_of_list component_module_decls$
              type param_names = $param_names_ctyp$
              type deep_config = $deep_config_ctyp$
              type template_data = unit
              let default_template_data ?default () = None
              let default_deep_config = $default_deep_config_expr$
              type repr = $repr_ctyp$
              let component_names = $Helpers.expr_list component_name_strings$
              let of_repr = $of_repr_expr$
              let to_repr = $to_repr_expr$
              let params_type = $params_type_expr$
              type ('arg, 'res) opt_component_configs_fun =
                $Helpers.Untranslate'.expr opt_component_configs_fun_type$
              let opt_component_configs_fun k =
                $exp:opt_component_configs_fun$
              let default_template = Deriving_Form.default_template
              let $lid:components_list_name$ :
                  (a, param_names, deep_config) $lid:component_type_name$ list =
                $fields_expr$
            end
            include $uid:make_module_name$ (Options)
          end
        >>
      | _ -> Base.fatal_error _loc "Form not available here"
    in
    fun decls ->
      Ast.stSem_of_list
        (List.map for_decl decls)

  let generate_sigs _ = <:sig_item< >>
end
