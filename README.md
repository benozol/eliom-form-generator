Deriving Eliom functionality from type definitions
==================================================

Derive easily Eliom functionality from record and ADT type declarations, like this:

```ocaml
type type_1 = A | B of int deriving Form
type type_2 = { i : int ; j : type_1 } deriving Form
```
The form content for `get_form` or `post_form` can then be generate with the function `content`:
```ocaml
  Eliom_content.Html5.F.get_form ~service
    (Form_type_2.content () ())
```
The description of Eliom's representation of the values is available through `params_type`:
```ocaml
  My_app.service ~path ~get_params:(Form_type_2.params_type "value") ()
```
`deriving-eliom-form` can even be used to display a given value:
```ocaml
  Form_type_2.display ~value () ()
```

NB that `deriving-eliom-form` works also well `type_conv`: together with [orm](https://github.com/mirage/orm) it forms a web programmer's dreamteam :-) !

Design
------

If you declare a type `t` with `deriving Form` (or `with form`), the most prominent generated functions are
 * ``Form_t.params_type prefix : (t, [`WithoutSuffix], param_names) Eliom_parameter.params_type``
 * ``Form_t.content () () : param_names -> Html5_types.form_content Html5.elt list``
 * ``Form_t.display ~value () () : Html5_types.form_content Html5.elt list``
 * ``Form_t.config () () : Form_t.config``

Why do the latter function take two times the value `()` as an argument ?
Those separate two kinds of optional arguments : 
The arguments for the local configuration and the arguments to configure all components -
i.e. the fields of a record or the variants of a ADT. For example
```ocaml
Form_type_2.content :
    ?label:Html5_types.form_content Eliom_content_core.Html5.elt list ->
    ?annotation:Html5_types.form_content Eliom_content_core.Html5.elt list ->
    ?default:type_2 ->
    ?classes:string list ->
    ?template:(type_2, param_names, template_data) Deriving_Form.Template.t ->
    ?template_data:template_data ->
    () ->
    ?i:Form_int.config ->
    ?j:Form_type1.config ->
    () ->
    param_names -> Html5_types.form_content Eliom_content_core.Html5.elt list
```
The arguments `label`, `annotation`, `default`, `classes`, `template`, `template_data` concern the
local presentation of the content, and how the presentations of the components are combined.
The arguments `i` and `j` are created by the functions `Form_int.config`
and `Form_type1.config` and concern just the presentation of the fields.

For example to have a special label for the overall form, and a default value `A` in the field `j`, do
```ocaml
let j = Form_type_1.config ~default:A () () in
Form_type_2.content ~label:[pcdata "i and j"] () ~j ()
```

Current limitation
------------------
Currently, `deriving-eliom-form` is restricted to very simple type definitions of records and ADTs
 - without recursion
 - without type variable
 - with only simple type constructors or unary type applications as components
