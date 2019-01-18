(** A terminal is a device capable of displaying one or more Emacs frames.

    [(Info-goto-node "(elisp)Multiple Terminals")] *)

open! Core_kernel
open! Import0
include Value.Subtype

(** [(describe-function 'terminal-list)] *)
val all_live : unit -> t list

(** [(describe-function 'terminal-name)] *)
val name : t -> string

val graphical : t -> bool
