(** Emacs "online" help.

    [(Info-goto-node "(emacs)Help")] *)

open! Core_kernel
open! Import

(** [(describe-function 'describe-function)]. *)
val describe_function : Symbol.t -> unit

(** [~obscure_symbol:true] is primarily used for testing, when the symbol is a gensym. *)
val describe_function_text : ?obscure_symbol:bool -> Symbol.t -> string

(** [(describe-function 'describe-key)]. *)
val describe_key : Key_sequence.t -> unit

val describe_key_text : Key_sequence.t -> string

(** [(describe-function 'describe-variable)]. *)
val describe_variable : Symbol.t -> unit

val describe_variable_text : Symbol.t -> string

(** [(describe-function 'describe-minor-mode)]. *)
val describe_minor_mode : Symbol.t -> unit

val describe_minor_mode_text : Symbol.t -> string

(** Return the key sequence bound to the given function. If multiple sequences are bound
    to the given function, return the shortest.

    [(describe-function 'where-is-internal)] *)
val where_is : Symbol.t -> Key_sequence.t option

val where_is_string : Symbol.t -> string
