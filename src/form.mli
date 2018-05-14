(** An Emacs "form", which is a Lisp object that is intended to be evaluated.
    [(Info-goto-node "(elisp)Forms")]. *)

open! Core_kernel
open! Import0

include Value.Subtype

(** [(describe-function 'eval)] *)
val eval   : t -> Value.t
val eval_i : t -> unit

(** [(describe-function 'read)] *)
val read : string -> t

(** [eval_string string] = [eval (read string)] *)
val eval_string : string -> Value.t

val nil : t

val string : string -> t

val symbol : Symbol.t -> t

val int : int -> t

val quote : Value.t -> t

val progn : t list -> t

val let_ : (Symbol.t * t) list -> t -> t

val lambda
  :  ?docstring     : string
  -> ?interactive   : string
  -> ?optional_args : Symbol.t list
  -> ?rest_arg      : Symbol.t
  -> Source_code_position.t
  -> args : Symbol.t list
  -> body : t
  -> t

(** [(describe-function 'defvar)]
    [(Info-goto-node "(elisp)Defining Variables")] *)
val defvar
  :  Source_code_position.t
  -> Symbol.t
  -> Value.t
  -> docstring : string
  -> unit

(** A function call, macro application, or special form.
    [(Info-goto-node "(elisp)Classifying Lists")]. *)
val list : t list -> t
