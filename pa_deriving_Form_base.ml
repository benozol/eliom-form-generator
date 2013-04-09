
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

  let component_module_decls  =
    List.map2
      (fun component_name (type_qname, type_params) ->
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
      | type_name, [], `Fresh (None, Type.Record record_fields, _), [], _ ->
        let component_names, component_types =
          let fields =
            List.map
              (fun (name, (poly_args, expr), _) ->
                if poly_args <> [] then
                  Base.fatal_error _loc "no polymorphic arguments allowed";
                name, get_simple_type expr)
              record_fields
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
          tuple_type
            (List.map (Helpers.Untranslate'.expr % type_expr_from_qname % add_prefix "param_names")
               component_module_names)
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
          tuple_type
            (List.map (Helpers.Untranslate'.expr % type_expr_from_qname % add_prefix "repr")
               component_module_names)
        in
        let tuple_expr =
          fold_right1
            (fun t sofar ->
              <:expr< $t$, $sofar$ >>)
            (List.map
               (fun field_name ->
                 <:expr< $lid:field_name$ >>)
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
        let tuple_pattern =
          fold_right1
            (fun t sofar ->
              <:patt< $t$, $sofar$ >>)
            (List.map
               (fun name -> <:patt< $lid:name$ >>)
               component_names)
        in
        let from_tuple_bindings =
          List.map
            (fun field_name ->
              field_name, <:expr< $uid:component_module_name field_name$ . from_repr $lid:field_name$ >>)
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
                 component_name,
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
              <:expr< fun ? $lid:component_name$ -> $sofar$ >>)
            component_names
            (<:expr< fun () arg -> k $tuple_expr$ arg >>)
        in
        let params_type =
          fold_right1
            (fun component_type sofar ->
              <:expr< Eliom_parameter.prod $component_type$ $sofar$ >>)
            (List.map
               (fun component_name ->
                 let suffix = "_"^component_name in
                 <:expr<
                  $uid:component_module_name component_name$ . params_type
                    (prefix ^ $str:suffix$ ) >>)
               component_names)
        in
        let fields_expr =
          let project = Printf.sprintf "project_%s" in
          let projection_pel =
            List.map
              (fun field_name ->
                <:patt< $lid:project field_name$ >>,
                <:expr< fun $tuple_pattern$ -> $lid:field_name$ >>)
              component_names
          in
          let component_module_decls =
            let component_module component_name =
              <:module_expr<
                struct
                  type enclosing_a = a
                  type enclosing_param_names = param_names
                  type enclosing_deep_config = deep_config
                  let project_default $Helpers.record_pattern record_fields$ =
                    Some ($lid:component_name$)
                  let project_param_names = $lid:project component_name$
                  let project_config = $lid:project component_name$
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
              let from_repr $pat:tuple_pattern$ =
                $Helpers.record_expr from_tuple_bindings$
              let to_repr $Helpers.record_pattern record_fields$ =
                $to_tuple_expr$
              let params_type prefix = $params_type$
              type ('arg, 'res) opt_component_configs_fun =
                $Helpers.Untranslate'.expr opt_component_configs_fun_type$
              let opt_component_configs_fun k =
                $exp:opt_component_configs_fun$
              let default_template = Deriving_Form.default_template
              let fields : (a, param_names, deep_config) field list =
                $fields_expr$
            end
            include Make_record (Options)
          end
        >>
      | _ -> Base.fatal_error _loc "Form not available here"
    in
    fun decls ->
      Ast.stSem_of_list
        (List.map for_decl decls)

  let generate_sigs _ = <:sig_item< >>
end
