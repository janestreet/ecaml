(** A minor mode provides optional features that users may enable or
    disable independently of the choice of major mode.  Minor modes can be
    enabled individually or in combination.

    [(Info-goto-node "(elisp)Minor Modes")] *)

open! Core_kernel
open! Import

type t =
  { function_name : Symbol.t
  ; variable_name : Symbol.t }
[@@deriving sexp_of]

val is_enabled : t -> bool

val disable : t -> unit
val enable  : t -> unit

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
