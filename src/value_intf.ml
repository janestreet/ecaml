(** [Value.t] is the OCaml type corresponding to Emacs's universal type of values.  It is
    represented as an OCaml custom block ([emacs_value_ops] in [ecaml_stubs.c]) wrapped
    around the [emacs_value] pointer type defined by the Emacs native-code module
    interface, [emacs-module.h], available in Emacs 25 and beyond.  This module has
    low-level functions for working with Emacs values, OCaml wrappers that call the C
    functions specified in [emacs-module.h].  All other calls from OCaml to Emacs are
    built on top of this module. *)

open! Core_kernel
open! Import

module type Make_subtype_arg = sig
  type value
  val here : Source_code_position.t
  val name : string
  val is_in_subtype : value -> bool
end

module type Funcall = sig
  type t
  type value

  val funcall0       : t -> value
  val funcall1       : t -> value -> value
  val funcall2       : t -> value -> value -> value
  val funcall3       : t -> value -> value -> value -> value
  val funcall4       : t -> value -> value -> value -> value -> value
  val funcall5       : t -> value -> value -> value -> value -> value -> value
  val funcallN       : t -> value list -> value
  val funcallN_array : t -> value array -> value

  val funcall0_i       : t -> unit
  val funcall1_i       : t -> value -> unit
  val funcall2_i       : t -> value -> value -> unit
  val funcall3_i       : t -> value -> value -> value -> unit
  val funcall4_i       : t -> value -> value -> value -> value -> unit
  val funcall5_i       : t -> value -> value -> value -> value -> value -> unit
  val funcallN_i       : t -> value list -> unit
  val funcallN_array_i : t -> value array -> unit

  val funcall_int_int_value_value_unit : t -> int -> int -> value -> value -> unit
  val funcall_int_int_value_unit : t -> int -> int -> value -> unit
end

module type Subtype = sig
  type value
  type 'a type_

  (** We expose [private value] for free identity conversions when the value is nested in
      some covariant type, e.g. [(symbols : Symbol.t list :> Value.t list)] rather than
      [List.map symbols ~f:Symbol.to_value]. *)
  type t = private value [@@deriving sexp_of]

  val of_value_exn : value -> t
  val to_value : t -> value

  (** [eq t1 t2 = Value.eq (to_value t1) (to_value t2)], i.e. [eq] checks whether the
      Emacs values underlying [t1] and [t2] are physically equal.  This is different than
      [phys_equal t1 t2], because we don't always wrap [eq] Emacs values in [phys_equal]
      OCaml values.  I.e. [phys_equal t1 t2] implies [eq t1 t2], but not the converse. *)
  val eq : t -> t -> bool

  val type_ : t type_
end

module type Value = sig

  type t = Value0.t [@@deriving sexp_of]

  include Funcall
    with type t := t
    with type value := t

  val intern : string -> t

  val nil : t
  val t   : t

  val list : t list -> t

  val cons : t -> t -> t (** [(describe-function 'cons)] *)

  val car_exn : t -> t (** [(describe-function 'car)] *)
  val cdr_exn : t -> t (** [(describe-function 'cdr)] *)

  val to_list_exn : t -> f:(t -> 'a) -> 'a list

  val type_of : t -> t

  (** - [(Info-goto-node "(elisp)Type Predicates")] *)
  val is_array                : t -> bool (** [(describe-function 'arrayp)] *)
  val is_buffer               : t -> bool (** [(describe-function 'bufferp)] *)
  val is_command              : t -> bool (** [(describe-function 'commandp)] *)
  val is_cons                 : t -> bool (** [(describe-function 'consp)] *)
  val is_event                : t -> bool (** [(describe-function 'eventp)] *)
  val is_float                : t -> bool (** [(describe-function 'floatp)] *)
  val is_font                 : t -> bool (** [(describe-function 'fontp)] *)
  val is_frame                : t -> bool (** [(describe-function 'framep)] *)
  val is_function             : t -> bool (** [(describe-function 'functionp)] *)
  val is_hash_table           : t -> bool (** [(describe-function 'hash-table-p)] *)
  val is_integer              : t -> bool (** [(describe-function 'integerp)] *)
  val is_keymap               : t -> bool (** [(describe-function 'keymapp)] *)
  val is_marker               : t -> bool (** [(describe-function 'markerp)] *)
  val is_nil                  : t -> bool (** [eq t nil] *)
  val is_not_nil              : t -> bool (** [(describe-function 'is-not-nil)] *)
  val is_process              : t -> bool (** [(describe-function 'processp)] *)
  val is_string               : t -> bool (** [(describe-function 'stringp)] *)
  val is_symbol               : t -> bool (** [(describe-function 'symbolp)] *)
  val is_syntax_table         : t -> bool (** [(describe-function 'syntax-table-p)] *)
  val is_timer                : t -> bool (** [(describe-function 'timerp)] *)
  val is_vector               : t -> bool (** [(describe-function 'vectorp)] *)
  val is_window               : t -> bool (** [(describe-function 'windowp)] *)
  val is_window_configuration : t -> bool (** [(describe-function 'window-configuration-p)] *)

  val eq : t -> t -> bool

  val equal : t -> t -> bool (** [(describe-function 'equal)] *)

  val of_bool : bool -> t
  val to_bool : t -> bool  (** [is_not_nil] *)

  val of_int_exn : int -> t
  val to_int_exn : t -> int

  val of_float     : float -> t
  val to_float_exn : t -> float

  val of_utf8_bytes     : string -> t
  val to_utf8_bytes_exn : t -> string

  val vec_get  : t -> int -> t
  val vec_set  : t -> int -> t -> unit
  val vec_size : t -> int

  val initialize_module : unit

  (** An ['a Type.t] is an isomorphism between ['a] and a subset of [Value.t]. *)
  module Type : sig
    type value
    type 'a t =
      { name         : Sexp.t
      ; of_value_exn : value -> 'a
      ; to_value     : 'a -> value }
    [@@deriving sexp_of]

    val bool   : bool   t
    val int    : int    t
    val string : string t
    val value  : value  t

    val list   : 'a t -> 'a list   t
    val option : 'a t -> 'a option t

    val alist  : 'a t -> 'b t -> ('a * 'b) list t
  end with type value := t

  module type Funcall          = Funcall          with type value := t
  module type Make_subtype_arg = Make_subtype_arg with type value := t

  module type Subtype = Subtype
    with type value := t
    with type 'a type_ := 'a Type.t

  module Make_subtype (Subtype : Make_subtype_arg) : Subtype

  module Expert : sig
    val raise_if_emacs_signaled : unit -> unit
    val non_local_exit_signal : exn -> unit
  end

  module Stat : sig
    type t =
      { emacs_free_performed : int
      ; emacs_free_scheduled : int }
    [@@deriving sexp_of]

    val now : unit -> t
    val diff : t -> t -> t
  end
end
