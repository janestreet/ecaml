(** An Emacs "form", which is a Lisp object that is intended to be evaluated.
    [(Info-goto-node "(elisp)Forms")]. *)

open! Core
open! Async_kernel
open! Import
include Value.Subtype

(** [(describe-function 'eval)] *)
val eval : t -> Value.t Deferred.t

val eval_i : t -> unit Deferred.t

(** [(describe-function 'read)] *)
val read : string -> t

(** Read the first complete Elisp form out of this buffer. *)
val read_buffer : Core.Buffer.t -> t

(** [eval_string string] = [eval (read string)] *)
val eval_string : string -> Value.t Deferred.t

val nil : t
val bool : bool -> t
val string : string -> t
val symbol : Symbol.t -> t
val int : int -> t
val quote : Value.t -> t
val quoted_symbol : Symbol.t -> t
val progn : t list -> t
val let_ : (Symbol.t * t) list -> t -> t

(** A function call, macro application, or special form.
    [(Info-goto-node "(elisp)Classifying Lists")]. *)
val list : t list -> t

(** For [sym] and [args], return (sym . args). *)
val apply : Symbol.t -> t list -> t

module Blocking : sig
  val eval : t -> Value.t
  val eval_i : t -> unit
  val eval_string : string -> Value.t
end
