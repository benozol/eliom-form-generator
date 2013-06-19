{server{
let debug fmt =
  Printf.ksprintf
    (fun str ->
      Ocsigen_messages.console (fun () -> str))
    fmt
}}
{client{
  let debug = Eliom_lib.debug
}}
{shared{
let failwith fmt =
  Printf.ksprintf
    (fun str ->
      debug "%s" str;
      failwith str)
    fmt
}}
{server{
  let message fmt =
    Printf.ksprintf (fun str -> Ocsigen_messages.console (fun () -> str)) fmt
}}
{client{
  let message fmt =
    Printf.ksprintf (fun str -> Firebug.console ## log (Js.string str)) fmt
}}

{client{
  let debug = Eliom_lib.debug
  let to_dom = Eliom_content.Html5.To_dom.of_element
}}

{shared{

type div_content = Html5_types.div_content Eliom_content.Html5.elt list
type form_content = Html5_types.form_content Eliom_content_core.Html5.elt list
type form_elt = Html5_types.form_content Eliom_content_core.Html5.elt
type button_content = Html5_types.button_content Eliom_content_core.Html5.elt list
type flow5 = Html5_types.flow5 Eliom_content_core.Html5.elt list
type pcdata = Html5_types.pcdata Eliom_content_core.Html5.elt
let pcdata = Eliom_content.Html5.F.pcdata
let ksprintf = Printf.ksprintf

let option_get ~default = function
  | Some x -> x
  | None -> default
let option_get' ~default = function
  | Some x -> x
  | None -> default ()
let option_map ~f = function
  | Some x -> Some (f x)
  | None -> None
let option_get_map : default:'b -> f:('a -> 'b) -> 'a option -> 'b =
  fun ~default ~f o ->
    option_get ~default (option_map ~f o)
let option_get'_map : default:(unit -> 'b) -> f:('a -> 'b) -> 'a option -> 'b =
  fun ~default ~f o ->
    option_get' ~default (option_map ~f o)
let option_iter ~f = function
  | Some x -> f x
  | None -> ()
let option_to_list = function
  | Some x -> [x]
  | None -> []
let option_bind ~f = function
  | Some x -> f x
  | None -> None
let maybe_get_option_map really opt f =
  if really then
    option_get ~default:[] (option_map ~f opt)
  else []
let some x = Some x
let is_some = function Some _ -> true | _ -> false
let from_some = function Some x -> x | _ -> failwith "from_some"
let option_or o1 o2 =
  if o1 <> None
  then o1
  else o2
let list_filter_some li =
  List.map (option_get' ~default:(fun () -> assert false))
    (List.filter (fun x -> x <> None) li)
let list_singleton x = [x]
let cons x xs = x :: xs

let option_get_lwt : default:(unit -> 'a Lwt.t) -> 'a option -> 'a Lwt.t =
  fun ~default option ->
    option_get' ~default
      (option_map ~f:Lwt.return option)

let map_fst ~f (a, b) = f a, b
let map_snd ~f (a, b) = a, f b

let identity x = x
let constant x _ = x
let flip f x y = f y x
let (//) = flip
let (-|) f g = fun x -> f (g x)
let (@) f x = f x
let (@@) = List.append

module type Monad = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  module List : sig
    val map : ('a -> 'b t) -> 'a list -> 'b list t
  end
end

module Identity = struct
  type 'a t = 'a
  let bind x f = f x
  let return x = x
  let map f x = f x
  module List = List
end

module Lwt' = struct
  include Lwt
  module List = struct
    let map = Lwt_list.map_p
  end
end

module String_map = struct
  include Map.Make (String)
  let get ~default key map =
    try find key map
    with Not_found -> default key
  let get_option key map =
    try Some (find key map)
    with Not_found -> None
  let rec find' key = function
    | [] -> raise Not_found
    | map :: maps ->
      try find key map
      with Not_found -> find' key maps
  let from_list li =
    List.fold_right (fun (key, value) -> add key value) li empty
  let from_options_list li =
    List.fold_right
      (fun (key, value) sofar ->
        match value with
        | Some value -> add key value sofar
        | None -> sofar)
      li empty
end
}}
