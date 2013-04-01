
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

  let generate : Type.decl list -> Ast.str_item =
    let for_decl : Type.decl -> Ast.str_item  = function
      | type_name, [], `Fresh (None, Type.Record record_fields, _), [], _ ->
        let form_module_name type_name = "Form_"^type_name in
        let field_names, types =
          List.split
            (List.map
               (function
                 | (field_name, ([], `Constr (type_qname, type_params)), _) ->
                   let type_params =
                     List.map
                       (function
                         | `Constr (type_qname, []) -> type_qname
                         | _ -> Base.fatal_error _loc "only simple type constructors")
                       type_params
                   in
                   field_name, (type_qname, type_params)
                 | _ -> Base.fatal_error _loc "Form not allowed here")
               record_fields)
        in
        let form_modules =
          List.map2
            (fun field_name (type_qname, type_params) ->
              let module_qname = map_last form_module_name type_qname in
              let me : Ast.module_expr =
                if type_params = [] then
                  <:module_expr< $id:ident_of_qname module_qname$ >>
                else
                  let module_qname_make = append "Make" module_qname in
                  List.fold_left
                    (fun sofar name ->
                      <:module_expr< $mexp:sofar$ ( $mexp:name$ ) >>)
                    (<:module_expr< $id:ident_of_qname module_qname_make$ >>)
                    (List.map
                       (fun type_param_qname ->
                         let id =
                           ident_of_qname
                             (append "Options"
                                (map_last form_module_name type_param_qname))
                         in
                         <:module_expr< $id:id$ >>)
                       type_params)
              in
              <:str_item< module $uid:form_module_name field_name$ = $mexp:me$ >>)
            field_names
            types
        in
        let form_module_names =
          List.map
            (function field_name ->
              [form_module_name field_name])
            field_names
        in
        let add_prefixes prefix qnames =
          List.map
            (fun qname ->
              prefix :: qname)
            qnames
        in
        let tuple_type =
          fold_right1
            (fun t sofar ->
              `Tuple [t; sofar])
        in
        let tuple_type' =
          fold_right1
            (fun t1 t2 ->
              <:ctyp< $t1$ * $t2$ >>)
        in
        let types = List.map (fun qname -> `Constr (List.rev qname, [])) in
        let param_names = tuple_type (types (add_prefixes "param_names" form_module_names)) in
        let deep_config =
          tuple_type'
            (List.map
               (fun typ ->
                 <:ctyp< $Helpers.Untranslate'.expr typ$ option >>)
               (types
                  (add_prefixes "config" form_module_names)))
        in
        let default_deep_config =
          fold_right1
            (fun none sofar ->
              Helpers.tuple_expr [none; sofar])
            (List.map (fun _ -> <:expr< None >>) form_module_names)
        in
        let tuple_type = tuple_type (types (add_prefixes "tuple" form_module_names)) in
        let field_module_name = Printf.sprintf "Field_%s" in
        let tuple_expr =
          fold_right1
            (fun t sofar ->
              <:expr< $t$, $sofar$ >>)
            (List.map
               (fun field_name ->
                 <:expr< $lid:field_name$ >>)
               field_names)
        in
        let to_tuple_expr =
          fold_right1
            (fun t sofar ->
              <:expr< $t$, $sofar$ >>)
            (List.map
               (fun field_name ->
                 <:expr< $uid:form_module_name field_name$ . to_tuple $lid:field_name$ >>)
               field_names)
        in
        let tuple_pattern =
          fold_right1
            (fun t sofar ->
              <:patt< $t$, $sofar$ >>)
            (List.map
               (fun name -> <:patt< $lid:name$ >>)
               field_names)
        in
        let from_tuple_bindings =
          List.map
            (fun field_name ->
              field_name, <:expr< $uid:form_module_name field_name$ . from_tuple $lid:field_name$ >>)
            field_names
        in
        let project = Printf.sprintf "project_%s" in
        let projections =
          List.map
            (fun field_name ->
              <:str_item<
                let $lid:project field_name$ $tuple_pattern$ = $lid:field_name$
                  >>)
            field_names
        in
        let field_modules =
          List.map2
            (fun field_name field_module ->
              <:str_item<
                module $uid:field_module_name field_name$ = struct
                  type enclosing_a = a
                  type enclosing_param_names = param_names
                  type enclosing_deep_config = deep_config
                  let project_default $Helpers.record_pattern record_fields$ =
                    Some ($lid:field_name$)
                  let project_param_names = $lid:project field_name$
                  let project_config = $lid:project field_name$
                  include $id:ident_of_qname field_module$
                end
              >>)
            field_names
            form_module_names
        in
        let field_module_exprs =
          List.map
            (fun field_name ->
              <:expr< (module $uid:field_module_name field_name$) >>)
            field_names
        in
        let field_name_strings =
          List.map
            (fun field_name -> <:expr< $str:field_name$ >>)
            field_names
        in
        let opt_field_configs_fun_type : Type.expr =
          List.fold_right
            (fun field_name sofar ->
              `Label
                (`Optional,
                 field_name,
                 `Constr ([form_module_name field_name; "config"], []),
                 sofar))
            field_names
            (`Function
                (`Constr (["unit"], []),
                 `Function (`Param ("arg", None), `Param ("res", None))))
        in
        let opt_field_configs_fun =
          List.fold_right
            (fun field_name sofar ->
              <:expr< fun ? $lid:field_name$ -> $sofar$ >>)
            field_names
            (<:expr< fun () arg -> k $tuple_expr$ arg >>)
        in
        let params_type =
          fold_right1
            (fun field_type sofar ->
              <:expr< Eliom_parameter.prod $field_type$ $sofar$ >>)
            (List.map
               (fun field_name ->
                 let suffix = "_"^field_name in
                 <:expr<
                  $uid:form_module_name field_name$ . params_type
                    (prefix ^ $str:suffix$ ) >>)
               field_names)
        in
        <:str_item<
          module $uid:form_module_name type_name$ = struct
            open Deriving_Form
            module Options = struct
              type a = $lid:type_name$
              ;;
              $Ast.stSem_of_list form_modules$
              type param_names = $Helpers.Untranslate'.expr param_names$
              type deep_config = $deep_config$
              let default_deep_config = $exp:default_deep_config$
              type tuple = $Helpers.Untranslate'.expr tuple_type$
              let field_names = $Helpers.expr_list field_name_strings$
              let from_tuple $pat:tuple_pattern$ =
                $Helpers.record_expr from_tuple_bindings$
              let to_tuple $Helpers.record_pattern record_fields$ =
                $to_tuple_expr$
              let params_type prefix = $params_type$
              type ('arg, 'res) opt_field_configs_fun =
                $Helpers.Untranslate'.expr opt_field_configs_fun_type$
              let opt_field_configs_fun k =
                $exp:opt_field_configs_fun$
              let default_template = Deriving_Form.default_template
              ;;
              $Ast.stSem_of_list projections$
              $Ast.stSem_of_list field_modules$
              let fields : (a, param_names, deep_config) field list =
                $Helpers.expr_list field_module_exprs$
            end
            include Make(Options)
          end
        >>
      | _ -> Base.fatal_error _loc "Form not available here"
    in
    fun decls ->
      Ast.stSem_of_list
        (List.map for_decl decls)

  let generate_sigs _ = <:sig_item< >>
end
