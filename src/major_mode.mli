(** Major modes specialize Emacs for editing particular kinds of text. Each buffer has
    only one major mode at a time.

    [(Info-goto-node "(elisp)Major Modes")] *)

open! Core
open! Async_kernel
open! Import

module Auto_mode : sig
  type t =
    | If_filename_matches of Regexp.t
    | If_filename_matches_then_delete_suffix_and_recur of Regexp.t
end

type t [@@deriving sexp_of]

include Equal.S with type t := t

(** Accessors *)

val symbol : t -> Symbol.t
val hook : t -> (Hook0.normal, unit) Hook0.t Or_error.t
val keymap : t -> Keymap.t Var.t

(** [wrap_existing mode_name] wraps the existing Emacs major mode named [mode_name], and
    stores it in the table of all major modes indexed by symbol. [wrap_existing] raises if
    a major mode associated with this symbol was already wrapped. *)
val wrap_existing : here:[%call_pos] -> string -> t

(** [find_or_wrap_existing] looks up the major mode associated with this symbol by a
    previous call to [wrap_existing] or creates one with the [Undistinguished] name. *)
val find_or_wrap_existing : here:[%call_pos] -> Symbol.t -> t

val change_to : t -> in_:Buffer.t -> unit Deferred.t

module Blocking : sig
  val change_to : t -> in_:Buffer.t -> unit
end

(** [(describe-function 'fundamental-mode)] [(Info-goto-node "(elisp)Major Modes")] *)
val fundamental : t

(** [(describe-function 'prog-mode)] [(Info-goto-node "(elisp)Basic Major Modes")] *)
val prog : t

(** [(describe-function 'special-mode)] [(Info-goto-node "(elisp)Basic Major Modes")] *)
val special : t

(** [(describe-function 'text-mode)] [(Info-goto-node "(elisp)Basic Major Modes")] *)
val text : t

(** [(describe-function 'tuareg-mode)] *)
val tuareg : t

(** [(describe-function 'makefile-mode)] *)
val makefile : t

(** [(describe-function 'lisp-data-mode)] *)
val lisp_data : t

(** [(describe-function 'scheme-mode)] *)
val scheme : t

(** [(describe-function 'emacs-lisp-mode)] *)
val emacs_lisp : t

(** [(describe-function 'asm-mode)] *)
val asm : t

(** [(describe-function 'python-mode)] *)
val python : t

(** [(describe-function 'image-mode)] *)
val image : t

(** [(describe-function 'define-derived-mode)] [(Info-goto-node "(elisp)Derived Modes")]

    Additionally, each [key_sequence, symbol] in [define_keys] is added to the new major
    mode's keymap. *)
val define_derived_mode
  :  ?auto_mode:Auto_mode.t
  -> Symbol.t
  -> Source_code_position.t
  -> docstring:string
  -> ?define_keys:(string * Symbol.t) list
  -> mode_line:string
  -> ?parent:t
  -> ?initialize:(unit, 'a) Defun.Returns.t * (unit -> 'a)
  -> unit
  -> t

val is_derived : t -> from:t -> bool

(** [(describe-variable 'major-mode)] *)
val major_mode_var : Symbol.t Buffer_local.t

module For_testing : sig
  val all_derived_modes : unit -> t list
end
