(** A minor mode provides optional features that users may enable or
    disable independently of the choice of major mode.  Minor modes can be
    enabled individually or in combination.

    [(Info-goto-node "(elisp)Minor Modes")] *)

open! Core_kernel
open! Import

type t =
  { function_name : Symbol.t
  ; variable_name : Symbol.t
  }
[@@deriving fields, sexp_of]

(** Returns true if [t] is defined and enabled, and false otherwise. *)
val is_enabled : t -> bool

val disable : t -> unit
val enable : t -> unit
val temporarily_disable : (unit, 'a) Sync_or_async.t -> t -> f:(unit -> 'a) -> 'a

(** [(describe-variable 'abbrev-mode)]
    [(describe-function 'abbrev-mode)] *)
val abbrev : t

(** [(describe-variable 'goto-address-mode)]
    [(describe-function 'goto-address-mode)]*)
val goto_address : t

(** [(describe-variable 'buffer-read-only)]
    [(describe-function 'read-only-mode)] *)
val read_only : t

(** [(describe-variable 'view-mode)]
    [(describe-function 'view-mode)] *)
val view : t

(** [(describe-variable 'visual-line-mode)]
    [(describe-function 'visual-line-mode)]*)
val visual_line : t

(** [(describe-variable 'url-handler-mode)]
    [(describe-function 'url-handler-mode)]*)
val url_handler : t

(** Find the keymap that is active when the given minor mode is enabled, if such a keymap
    exists.

    [(describe-variable 'minor-mode-map-alist)]*)
val keymap : t -> Keymap.t option

val keymap_exn : t -> Keymap.t

(** [(describe-function 'define-minor-mode)]
    [(Info-goto-node "(elisp)Defining Minor Modes")]

    Additionally, each [key_sequence, symbol] in [define_keys] is added to the new minor
    mode's keymap. *)
val define_minor_mode
  :  Symbol.t
  -> Source_code_position.t
  -> docstring:string
  -> ?define_keys:(string * Symbol.t) list
  -> ?mode_line:string
  -> global:bool
  -> ?initialize:(unit -> unit)
  -> unit
  -> t

module Private : sig
  val all_minor_modes : unit -> t list
end
