(** Emacs "online" help.

    [(Info-goto-node "(emacs)Help")] *)

open! Core_kernel
open! Import

(** [(describe-function 'describe-function)]. *)
val describe_function : Symbol.t -> unit

(** [~obscure_symbol:true] is primarily used for testing, when the symbol is a gensym. *)
val describe_function_text : ?obscure_symbol:bool -> Symbol.t -> string

(** [(describe-function 'describe-variable)]. *)
val describe_variable : Symbol.t -> unit

val describe_variable_text : Symbol.t -> string

(** [(describe-function 'describe-minor-mode)]. *)
val describe_minor_mode : Symbol.t -> unit

val describe_minor_mode_text : Symbol.t -> string
