(** An Emacs "form", which is a Lisp object that is intended to be evaluated.
    [(Info-goto-node "(elisp)Forms")]. *)

open! Core_kernel
open! Import

type t [@@deriving sexp_of]

(** Emacs forms are values, and vice versa, so [of_value] and [to_value] are implemented
    as the identity function. *)
val of_value : Value.t -> t
val to_value : t -> Value.t

(** [(describe-function 'eval)] *)
val eval : t -> Value.t

(** [(describe-function 'read)] *)
val read : string -> t

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
  -> Source_code_position.t
  -> args : Symbol.t list
  -> body : t
  -> t

(** A function call, macro application, or special form.  [(Info-goto-node
    "(elisp)Classifying Lists")]. *)
val list : t list -> t
