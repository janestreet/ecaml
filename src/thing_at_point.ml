open! Core_kernel
open! Import

module Q = struct
  include Q
  let beginning_of_thing               = "beginning-of-thing"               |> Symbol.intern
  let bounds_of_thing_at_point         = "bounds-of-thing-at-point"         |> Symbol.intern
  let email                            = "email"                            |> Symbol.intern
  let end_of_thing                     = "end-of-thing"                     |> Symbol.intern
  let filename                         = "filename"                         |> Symbol.intern
  let forward_thing                    = "forward-thing"                    |> Symbol.intern
  let line                             = "line"                             |> Symbol.intern
  let page                             = "page"                             |> Symbol.intern
  let sentence                         = "sentence"                         |> Symbol.intern
  let thing_at_point                   = "thing-at-point"                   |> Symbol.intern
  let thing_at_point_file_name_chars   = "thing-at-point-file-name-chars"   |> Symbol.intern
  let url                              = "url"                              |> Symbol.intern
  let whitespace                       = "whitespace"                       |> Symbol.intern
  let word                             = "word"                             |> Symbol.intern
end

module F = struct
  open Value.Type
  open Funcall

  let beginning_of_thing =
    Q.beginning_of_thing <: Symbol.type_ @-> return nil
  let bounds_of_thing_at_point =
    Q.bounds_of_thing_at_point
    <: Symbol.type_ @-> return (option (tuple Position.type_ Position.type_))
  let end_of_thing =
    Q.end_of_thing <: Symbol.type_ @-> return nil
  let forward_thing =
    Q.forward_thing <: Symbol.type_ @-> int  @-> return bool
  let thing_at_point =
    Q.thing_at_point <: Symbol.type_ @-> bool @-> return (option Text.type_)
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
  | Defun       -> Q.defun
  | Email       -> Q.email
  | Filename    -> Q.filename
  | Line        -> Q.line
  | List        -> Q.list
  | Number      -> Q.number
  | Other sym   -> sym
  | Page        -> Q.page
  | Sentence    -> Q.sentence
  | Sexp        -> Q.sexp
  (* [chars] is passed separately through the [file_name_chars] variable. *)
  | String_of _ -> Q.filename
  | Symbol      -> Q.symbol
  | Url         -> Q.url
  | Whitespace  -> Q.whitespace
  | Word        -> Q.word
;;

let file_name_chars = Var.create Q.thing_at_point_file_name_chars Value.Type.string

let with_settings t ~f =
  match t with
  | String_of { chars } ->
    Current_buffer.set_value_temporarily file_name_chars chars ~f
  | _ ->
    f ()
;;

let find ?(text_properties=false) thing =
  with_settings thing ~f:(fun () ->
    F.thing_at_point (thing |> to_symbol) (not text_properties))
;;

let forward ?(n=1) thing =
  with_settings thing ~f:(fun () -> F.forward_thing (thing |> to_symbol) n)

let bounds thing =
  with_settings thing ~f:(fun () -> F.bounds_of_thing_at_point (thing |> to_symbol))

let did_not_raise f x =
  match f x with
  | _ -> true
  | exception _ -> false
;;

let beginning_exn thing =
  with_settings thing ~f:(fun () -> F.beginning_of_thing (thing |> to_symbol))

let beginning = did_not_raise beginning_exn

let end_exn thing =
  with_settings thing ~f:(fun () -> F.end_of_thing (thing |> to_symbol))

let end_ = did_not_raise end_exn

let bounds_prop  = Symbol.Property.create Q.bounds_of_thing_at_point Function.type_

let defthing
      ~(bounds : unit -> (Position.t * Position.t) option)
      loc
      symbol =
  Symbol.Property.put
    bounds_prop
    symbol
    (Defun.lambda_nullary loc
       Value.Type.(option (tuple Position.type_ Position.type_))
       bounds);
;;
