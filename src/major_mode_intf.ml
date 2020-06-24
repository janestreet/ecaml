(** Major modes specialize Emacs for editing particular kinds of text.  Each buffer has
    only one major mode at a time.

    [(Info-goto-node "(elisp)Major Modes")] *)

open! Core_kernel
open! Async_kernel
open! Import
module Hook = Hook0

module type S = sig
  type t
  type name = ..
  type name += Major_mode

  val major_mode : t
  val enabled_in_current_buffer : unit -> bool
end

module type Major_mode = sig
  type t [@@deriving sexp_of]

  include Equal.S with type t := t

  module Compare_by_name : sig
    type nonrec t = t [@@deriving compare, equal, hash, sexp_of]
  end

  (** Accessors *)
  val symbol : t -> Symbol.t

  module Name : sig
    (** Names let us pattern-match on major modes. *)
    type t = ..

    (** Dummy value for modes we don't care about matching. *)
    type t += Undistinguished
  end

  val name : t -> Name.t
  val hook : t -> Hook.normal Hook.t
  val keymap : t -> Keymap.t
  val keymap_var : t -> Keymap.t Var.t
  val syntax_table : t -> Syntax_table.t

  module type S = S with type t := t and type name := Name.t

  (** [wrap_existing mode_name] wraps the existing Emacs major mode named [mode_name], and
      stores it in the table of all major modes indexed by symbol. [wrap_existing] raises
      if a major mode associated with this symbol was already wrapped. *)
  val wrap_existing : string -> Source_code_position.t -> (module S)

  (** [find_or_wrap_existing] looks up the major mode associated with this symbol by a
      previous call to [wrap_existing] or creates one with the [Undistinguished] name. *)
  val find_or_wrap_existing : Source_code_position.t -> Symbol.t -> t

  val change_to : t -> in_:Buffer.t -> unit Deferred.t

  (** [(describe-function 'fundamental-mode)]
      [(Info-goto-node "(elisp)Major Modes")] *)
  module Fundamental : S

  (** [(describe-function 'prog-mode)]
      [(Info-goto-node "(elisp)Basic Major Modes")] *)
  module Prog : S

  (** [(describe-function 'special-mode)]
      [(Info-goto-node "(elisp)Basic Major Modes")] *)
  module Special : S

  (** [(describe-function 'text-mode)]
      [(Info-goto-node "(elisp)Basic Major Modes")] *)
  module Text : S

  (** [(describe-function 'dired-mode)] *)
  module Dired : S

  (** [(describe-function 'tuareg-mode)] *)
  module Tuareg : S

  (** [(describe-function 'makefile-mode)] *)
  module Makefile : S

  (** [(describe-function 'lisp-mode)] *)
  module Lisp : S

  (** [(describe-function 'scheme-mode)] *)
  module Scheme : S

  (** [(describe-function 'emacs-lisp-mode)] *)
  module Emacs_lisp : S

  (** [(describe-function 'define-derived-mode)]
      [(Info-goto-node "(elisp)Derived Modes")]

      Additionally, each [key_sequence, symbol] in [define_keys] is added to the new major
      mode's keymap. *)
  val define_derived_mode
    :  Symbol.t
    -> Source_code_position.t
    -> docstring:string
    -> ?define_keys:(string * Symbol.t) list
    -> mode_line:string
    -> ?parent:t
    -> ?initialize:(unit, 'a) Defun.Returns.t * (unit -> 'a)
    -> unit
    -> (module S)

  val is_derived : t -> from:t -> bool

  (** [(describe-variable 'major-mode)] *)
  val major_mode_var : Symbol.t Buffer_local.t

  module For_testing : sig
    val all_derived_modes : unit -> t list
  end
end

