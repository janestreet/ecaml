(** An Emacs expression. *)

open! Core
open! Async
open! Import

type t [@@deriving sexp_of]

(** Emacs expressions are values, and vice versa, so [of_value] and [to_value] are
    implemented as the identity function. *)
val of_value : Value.t -> t
val to_value : t -> Value.t

(** Evaluate an expression, using the Emacs [eval] function. *)
val eval : t -> Value.t

(** Convert a string to an expression, using the Emacs [read] function. *)
val of_string : string -> t

val string : string -> t

val symbol : Symbol.t -> t

val apply : t -> t -> t

val quote : t -> t

val progn : t list -> t

val lambda
  :  ?docstring     : string
  -> ?interactive   : string
  -> ?optional_args : Symbol.t list
  -> ?rest_arg      : Symbol.t
  -> unit
  -> args : Symbol.t list
  -> body : t
  -> t

(** Generally, a function call, macro application, or syntax form. *)
val combination : t list -> t
