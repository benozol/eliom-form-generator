{shared{

  type 'a pathed_config

  (** {1 Generate Eliom form content from runtime type representation} *)
  val content :
    'a Deriving_Typerepr.t ->
    ?configs:('a pathed_config list) ->
    [ `One of 'a Eliom_parameter.caml ] Eliom_parameter.param_name ->
    Html5_types.form_content Eliom_content.Html5.elt

  type 'a config
  type 'a value
  type 'a template
  val string_widget :
    (?value:[`Default of string|`Constant of string] ->
     string Eliom_parameter.setoneradio Eliom_parameter.param_name ->
     Html5_types.span_content Eliom_content.Html5.elt) ->
    string template

  (** Auxiliary function for the construction of the [configs] parameter *)
  module Pathed_config : sig

    val default : 'a -> 'a value
    val constant : 'a -> 'a value
    val config :
      ?value:'a value ->
      ?label:string ->
      ?a:Html5_types.div_attrib Eliom_content.Html5.F.attrib list ->
      ?template:'a template ->
      unit -> [> `Config of 'a config ]
    val tree : 'a pathed_config list -> [> `Tree of 'a pathed_config list ]

    open Deriving_Typerepr
    val (/) : ('a, 'b) p -> ('b, 'c) p -> ('a, 'c) p
    val (-->) : ('a, 'b) p -> [ `Config of 'b config | `Tree of 'b pathed_config list ] -> 'a pathed_config

    include module type of Deriving_Typerepr.Path
  end

  (** Name of the installed CSS file *)
  val css_filename : string

  (** HTML class names used to generate form content *)
  val form_outmost_class : string
  val form_class : string
  val atomic_class : string
  val option_class : string
  val option_selector_class : string
  val option_content_class : string
  val sum_class : string
  val sum_selector_class : string
  val sum_content_class : string
  val sum_case_class : string
  val sum_case_marker_class : string -> string
  val tuple_class : string
  val record_class : string
  val record_field_marker_class : string -> string
  val marker_class : string
  val selector_snippet : string
  val content_snippet : string
  val list_class : string
  val list_item_class : string
  val button_add_class : string
  val button_remove_class : string
  val label_class : string

  val json_of_typerepr : 'a Deriving_Typerepr.t -> 'a Deriving_Json.t

}}

{client{
  (** Initializes the generated form content.
      To call, if the the content doesn't arrive in a form as part of the
      DOM from the server. *)
  val init_form : Dom_html.formElement Js.t -> unit
}}
