open! Core_kernel
open! Import

module Q = struct
  include Q

  let bounds_of_thing_at_point = "bounds-of-thing-at-point" |> Symbol.intern
  let email = "email" |> Symbol.intern
  let filename = "filename" |> Symbol.intern
  let line = "line" |> Symbol.intern
  let page = "page" |> Symbol.intern
  let sentence = "sentence" |> Symbol.intern
  let url = "url" |> Symbol.intern
  let whitespace = "whitespace" |> Symbol.intern
  let word = "word" |> Symbol.intern
end

type t =
  | Defun
  | Email
  | Filename
  | Line
  | List
  | Number
  | Other of Symbol.t
  | Page
  | Sentence
  | Sexp
  | String_of of { chars : string }
  | Symbol
  | Url
  | Whitespace
  | Word
[@@deriving sexp_of]

(* Not giving [t] a [type_] because it's not quite the same as what gets
   passed to [thing-at-point]. In particular, [Custom_chars] means to pass
   ['filename] to [thing-at-point] but set [thing-at-point-file-name-chars]
   first. *)

let to_symbol = function
  | Defun -> Q.defun
  | Email -> Q.email
  | Filename -> Q.filename
  | Line -> Q.line
  | List -> Q.list
  | Number -> Q.number
  | Other sym -> sym
  | Page -> Q.page
  | Sentence -> Q.sentence
  | Sexp -> Q.sexp
  (* [chars] is passed separately through the [file_name_chars] variable. *)
  | String_of _ -> Q.filename
  | Symbol -> Q.symbol
  | Url -> Q.url
  | Whitespace -> Q.whitespace
  | Word -> Q.word
;;

let file_name_chars = Var.Wrap.("thing-at-point-file-name-chars" <: string)

let with_settings t ~f =
  match t with
  | String_of { chars } ->
    Current_buffer.set_value_temporarily Sync file_name_chars chars ~f
  | _ -> f ()
;;

let thing_at_point =
  Funcall.Wrap.("thing-at-point" <: Symbol.t @-> bool @-> return (nil_or Text.t))
;;

let find ?(text_properties = false) thing =
  with_settings thing ~f:(fun () ->
    thing_at_point (thing |> to_symbol) (not text_properties))
;;

let forward_thing = Funcall.Wrap.("forward-thing" <: Symbol.t @-> int @-> return bool)

let forward ?(n = 1) thing =
  with_settings thing ~f:(fun () -> forward_thing (thing |> to_symbol) n)
;;

let bounds_of_thing_at_point =
  Funcall.Wrap.(
    "bounds-of-thing-at-point"
    <: Symbol.t @-> return (nil_or (tuple Position.t Position.t)))
;;

let bounds thing =
  with_settings thing ~f:(fun () -> bounds_of_thing_at_point (thing |> to_symbol))
;;

let did_not_raise f x =
  match f x with
  | _ -> true
  | exception _ -> false
;;

let beginning_of_thing = Funcall.Wrap.("beginning-of-thing" <: Symbol.t @-> return nil)

let beginning_exn thing =
  with_settings thing ~f:(fun () -> beginning_of_thing (thing |> to_symbol))
;;

let beginning = did_not_raise beginning_exn
let end_of_thing = Funcall.Wrap.("end-of-thing" <: Symbol.t @-> return nil)
let end_exn thing = with_settings thing ~f:(fun () -> end_of_thing (thing |> to_symbol))
let end_ = did_not_raise end_exn
let bounds_prop = Symbol.Property.create Q.bounds_of_thing_at_point Function.t

let defthing symbol loc ~(bounds : unit -> (Position.t * Position.t) option) =
  Symbol.Property.put
    bounds_prop
    symbol
    (Defun.lambda_nullary
       loc
       (Returns Value.Type.(nil_or (tuple Position.t Position.t)))
       bounds);
  Other symbol
;;
