(** Major modes specialize Emacs for editing particular kinds of text. Each buffer has
    only one major mode at a time.

    [(Info-goto-node "(elisp)Major Modes")] *)

open! Core
open! Async_kernel
open! Import
module Hook = Hook0

module Auto_mode = struct
  type t =
    | If_filename_matches of Regexp.t
    | If_filename_matches_then_delete_suffix_and_recur of Regexp.t
end

module Name = struct
  (** Names let us pattern-match on major modes. Doing so is discouraged, because it
      breaks normal, widely-used extension mechanisms ([define-derived-mode]). *)
  type t = ..

  (** Dummy value for modes we don't care about matching. *)
  type t += Undistinguished
end

module Intf (T : T) = struct
  open T

  module type S_with_lazy_keymap = sig
    type Name.t += Major_mode

    val major_mode : t
    val keymap : Keymap.t Lazy.t
    val enabled_in_current_buffer : unit -> bool
  end

  module type S = sig
    include S_with_lazy_keymap

    val keymap : Keymap.t
  end
end

module type Major_mode = sig
  module Auto_mode = Auto_mode
  module Name = Name

  type t [@@deriving sexp_of]

  include Equal.S with type t := t

  module Compare_by_name : sig
    type nonrec t = t [@@deriving compare, equal, hash, sexp_of]
  end

  include module type of Intf (struct
      type nonrec t = t
    end)

  (** Accessors *)

  val symbol : t -> Symbol.t
  val name : t -> Name.t
  val hook : t -> (Hook.normal, unit) Hook.t Or_error.t
  val keymap : t -> Keymap.t
  val keymap_var : t -> Keymap.t Var.t
  val syntax_table : t -> Syntax_table.t

  (** [wrap_existing mode_name] wraps the existing Emacs major mode named [mode_name], and
      stores it in the table of all major modes indexed by symbol. [wrap_existing] raises
      if a major mode associated with this symbol was already wrapped. *)
  val wrap_existing : ?here:Stdlib.Lexing.position -> string -> (module S)

  (** [wrap_existing_with_lazy_keymap] is like [wrap_existing], except the resulting
      module's [keymap] value has type [Keymap.t Lazy.t] rather than [Keymap.t]. This is
      needed if the keymap value isn't defined at the point that the major mode is
      wrapped. *)
  val wrap_existing_with_lazy_keymap
    :  ?here:Stdlib.Lexing.position
    -> string
    -> (module S_with_lazy_keymap)

  (** [find_or_wrap_existing] looks up the major mode associated with this symbol by a
      previous call to [wrap_existing] or creates one with the [Undistinguished] name. *)
  val find_or_wrap_existing : ?here:Stdlib.Lexing.position -> Symbol.t -> t

  val change_to : t -> in_:Buffer.t -> unit Deferred.t

  module Blocking : sig
    val change_to : t -> in_:Buffer.t -> unit
  end

  (** [(describe-function 'fundamental-mode)] [(Info-goto-node "(elisp)Major Modes")] *)
  module Fundamental : S_with_lazy_keymap

  (** [(describe-function 'prog-mode)] [(Info-goto-node "(elisp)Basic Major Modes")] *)
  module Prog : S

  (** [(describe-function 'special-mode)] [(Info-goto-node "(elisp)Basic Major Modes")] *)
  module Special : S

  (** [(describe-function 'text-mode)] [(Info-goto-node "(elisp)Basic Major Modes")] *)
  module Text : S

  (** [(describe-function 'tuareg-mode)] *)
  module Tuareg : S_with_lazy_keymap

  (** [(describe-function 'makefile-mode)] *)
  module Makefile : S_with_lazy_keymap

  (** [(describe-function 'lisp-data-mode)] *)
  module Lisp_data : S

  (** [(describe-function 'scheme-mode)] *)
  module Scheme : S_with_lazy_keymap

  (** [(describe-function 'emacs-lisp-mode)] *)
  module Emacs_lisp : S

  (** [(describe-function 'asm-mode)] *)
  module Asm : S_with_lazy_keymap

  (** [(describe-function 'python-mode)] *)
  module Python : S_with_lazy_keymap

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
    -> (module S)

  val is_derived : t -> from:t -> bool

  (** [(describe-variable 'major-mode)] *)
  val major_mode_var : Symbol.t Buffer_local.t

  module For_testing : sig
    val all_derived_modes : unit -> t list
  end
end
