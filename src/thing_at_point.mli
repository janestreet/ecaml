(** Support for detecting something at point---a filename, URL, what-have-you. *)

open! Core_kernel
open! Import

type t =
  | Defun
  | Email
  | Filename
  | Line
  | List
  | Number
  | Page
  | Sentence
  | Sexp
  (** Any string containing only characters in [chars], which is a regexp character
      alternative (i.e. a string that would go between square brackets in a regexp). *)
  | String_of of { chars : string }
  | Symbol
  | Url
  | Whitespace
  | Word
[@@deriving sexp_of]

(** Find the given kind of thing at point, if any. If [text_properties] is false (which is
    the default), text properties are stripped from the returned string.

    [(describe-function 'thing-at-point)] *)
(* Defaulting [text_properties] to [false] is consistent with
   [Current_buffer.contents]. *)
val find : ?text_properties : bool -> t -> Text.t option

(** [(describe-function 'forward-thing)] *)
val forward : ?n : int -> t -> unit

(** [(describe-function 'bounds-of-thing-at-point)] *)
val bounds : t -> (Position.t * Position.t) option

(** [(describe-function 'beginning-of-thing)] *)
val beginning : t -> unit

(** [(describe-function 'end-of-thing)] *)
val end_ : t -> unit
