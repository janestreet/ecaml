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
  (** Any other "thing" supported by [thing-at-point]. Use [defthing] to register a new
      thing type. *)
  | Other of Symbol.t
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

(** Find the given kind of thing at point, if any. If [text_properties] is false, text
    properties are stripped from the returned string.

    [(describe-function 'thing-at-point)] *)
val find
  :  ?text_properties : bool (** default: false *)
  -> t
  -> Text.t option

(** [(describe-function 'forward-thing)]

    NOTE: Many thing types, including [Other] and several built-in types, don't support
    [forward]. *)
val forward : ?n : int -> t -> bool

(** [(describe-function 'bounds-of-thing-at-point)] *)
val bounds : t -> (Position.t * Position.t) option

(** [(describe-function 'beginning-of-thing)] *)
val beginning_exn : t -> unit
(** [beginning] tries [beginning_exn] and returns false if it would have raised. *)
val beginning : t -> bool

(** [(describe-function 'end-of-thing)] *)
val end_exn : t -> unit
(** [end_] tries [end_exn] and returns false if it would have raised. *)
val end_ : t -> bool

(** Define [symbol] as a "thing" so that [Other symbol] works as the argument to [find],
    etc (but not [forward]). The effect is to define a property of the symbol; so long as
    that property isn't otherwise used, the symbol can be used for other purposes as well.
*)
val defthing
  (** An implementation of [bounds]. *)
  :  bounds:(unit -> (Position.t * Position.t) option)
  -> Source_code_position.t
  -> Symbol.t
  -> unit
