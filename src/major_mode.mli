(** Major modes specialize Emacs for editing particular kinds of text.  Each buffer has
    only one major mode at a time.

    [(Info-goto-node "(elisp)Major Modes")] *)

open! Core_kernel
open! Import

type t [@@deriving sexp_of]

include Equal.S with type t := t

(** Accessors *)
val change_command : t -> Symbol.t
val keymap         : t -> Keymap.t
val keymap_var     : t -> Keymap.t Var.t
val syntax_table   : t -> Syntax_table.t

val create : change_command:Symbol.t -> t

(** [(describe-function 'fundamental-mode)]
    [(Info-goto-node "(elisp)Major Modes")] *)
val fundamental : t

(** [(describe-function 'prog-mode)]
    [(Info-goto-node "(elisp)Basic Major Modes")] *)
val prog : t

(** [(describe-function 'special-mode)]
    [(Info-goto-node "(elisp)Basic Major Modes")] *)
val special : t

(** [(describe-function 'text-mode)]
    [(Info-goto-node "(elisp)Basic Major Modes")] *)
val text : t

(** [(describe-function 'define-derived-mode)]
    [(Info-goto-node "(elisp)Derived Modes")] *)
val define_derived_mode
  :  ?parent : t
  -> Source_code_position.t
  -> change_command : Symbol.t
  -> docstring      : string
  -> initialize     : (unit -> unit)
  -> mode_line      : string
  -> t
