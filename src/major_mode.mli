(** Major modes specialize Emacs for editing particular kinds of text.  Each buffer has
    only one major mode at a time.

    [(Info-goto-node "(elisp)Major Modes")] *)

open! Core_kernel
open! Import

type t [@@deriving sexp_of]

include Equal.S with type t := t

(** Accessors *)
val change_command : t -> Symbol.t

module Name : sig
  (** Names let us pattern-match on major modes. *)
  type t = ..

  (** Dummy value for modes we don't care about matching. *)
  type t += Undistinguished
end

val name : t -> Name.t
val keymap : t -> Keymap.t
val keymap_var : t -> Keymap.t Var.t
val syntax_table : t -> Syntax_table.t

(** If a name is provided, [create] associates [change_command] with this name. It is an
    error to associate a [change_command] more than once.

    If no name is provided, [create] looks up the name associated with this
    [change_command] by a previous call to [create], or uses [Undistinguished]. *)
val create : Source_code_position.t -> Name.t option -> change_command:Symbol.t -> t

(** [(describe-function 'fundamental-mode)]
    [(Info-goto-node "(elisp)Major Modes")] *)
type Name.t += Fundamental

val fundamental : t

(** [(describe-function 'prog-mode)]
    [(Info-goto-node "(elisp)Basic Major Modes")] *)
type Name.t += Prog

val prog : t

(** [(describe-function 'special-mode)]
    [(Info-goto-node "(elisp)Basic Major Modes")] *)
type Name.t += Special

val special : t

(** [(describe-function 'text-mode)]
    [(Info-goto-node "(elisp)Basic Major Modes")] *)
type Name.t += Text

val text : t

(** [(describe-function 'define-derived-mode)]
    [(Info-goto-node "(elisp)Derived Modes")]

    Additionally, each [key_sequence, symbol] in [define_keys] is added to the new major
    mode's keymap. *)
val define_derived_mode
  :  ?define_keys:(string * Symbol.t) list
  -> ?parent:t
  -> Source_code_position.t
  -> Name.t
  -> change_command:Symbol.t
  -> docstring:string
  -> initialize:(unit -> unit)
  -> mode_line:string
  -> t

module For_testing : sig
  (** After this lazy is forced, [define_derived_mode] will raise. *)
  val finish_deriving_modes : t list Lazy.t
end
