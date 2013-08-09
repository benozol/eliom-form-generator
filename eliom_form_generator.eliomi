{shared{

  type 'a pathed_config

  (** {1 Generate Eliom form content from runtime type representation} *)
  val content :
    'a Deriving_Typerepr.t ->
    ?configs:('a pathed_config list) ->
    [ `One of 'a Eliom_parameter.caml ] Eliom_parameter.param_name ->
    Html5_types.form_content Eliom_content.Html5.F.elt

  type 'a config

  (** Auxiliary function for the construction of the [configs] parameter *)
  module Pathed_config : sig

    val default : 'a -> [> `Default of 'a ]
    val constant : 'a -> [> `Constant of 'a ]
    val config : ?value:[ `Default of 'a | `Constant of 'a ] -> ?label:string ->
      ?a:Html5_types.div_attrib Eliom_content.Html5.F.attrib list -> unit -> 'a config

    open Deriving_Typerepr
    val (-->) : ('a, 'b) p -> 'b config -> 'a pathed_config
    val (/) : ('a, 'b) p -> (('a, 'b) p -> ('a, 'c) p) -> ('a, 'c) p
    val root : ('a, 'a) p
    val list_item : int -> ('a, 'b list) p -> ('a, 'b) p
    val some : ('a, 'b option) p -> ('a, 'b) p
    val nullary_case : 'b t -> string -> ('a, 'b) p -> ('a, unit) p
    val unary_case : 'b t -> string -> 'c t -> ('a, 'b) p -> ('a, 'c) p
    val nary_case : 'b t -> string -> 'c t -> ('a, 'b) p -> ('a, 'c) p
    val component : 'b t -> int -> 'c t -> ('a, 'b) p -> ('a, 'c) p
    val field : 'b t -> string -> 'c t -> ('a, 'b) p -> ('a, 'c) p
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
