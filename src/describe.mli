open! Core_kernel
open! Import

(** Emacs "online" help. *)

(** [(describe-function 'describe-function)].

    [~obscure_symbol:true] is primarily used for testing, when the symbol is a gensym. *)
val function_ : ?obscure_symbol:bool -> Symbol.t -> string

(** [(describe-function 'describe-variable)]. *)
val variable : Symbol.t -> string

(** [(describe-function 'describe-minor-mode)]. *)
val minor_mode : Symbol.t -> string

val display_minor_mode : Symbol.t -> unit
