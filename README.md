Deriving Eliom functionality from type definitions
==================================================

Derive easily Eliom form content from record and ADT type declarations, like this:

```ocaml
type type_1 = A | B of int deriving Form
type type_2 = { i : int option ; j : type_1 } deriving Form

let service = My_app.service ~path ~get_params:(Form_type_2.params_type "value") ()

let _ = My_app.register_service ~path:[""] ~get_params:Eliom_parameter.unit
  (fun () () ->
    Lwt.return
      (Eliom_tools.Html5.F.html ~title
          (Eliom_content.Html5.F.get_form ~service
             (Form_type_2.content () ()))))
```

It can even be used to display values:
```ocaml
let () =
  My_app.register ~service
    (Form_my_other_type.get_handler
       (fun value () ->
          Lwt.return
            (Eliom_tools.Html5.F.html ~title
               (Form_type_2.display ~value () ()))))
```

NB that `deriving-eliom-synatx` works also well `type_conv`: `deriving-eliom-form.syntax_tc` and `orm` is the web programmer's dreamteam!
