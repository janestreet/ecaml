(** [Value.t] is the OCaml type corresponding to Emacs's universal type of values.  It is
    represented as an OCaml custom block ([emacs_value_ops] in [ecaml_stubs.c]) wrapped
    around the [emacs_value] pointer type defined by the Emacs native-code module
    interface, [emacs-module.h], available in Emacs 25 and beyond.  This module has
    low-level functions for working with Emacs values, OCaml wrappers that call the C
    functions specified in [emacs-module.h].  All other calls from OCaml to Emacs are
    built on top of this module. *)

open! Core
open! Async
open! Import

type t = Value0.t [@@deriving sexp_of]

val funcall   : t -> t list -> t
val funcall_i : t -> t list -> unit

val intern : string -> t

val nil : t

val list : t list -> t

val type_of : t -> t

val is_not_nil : t -> bool

val is_symbol : t -> bool

val eq : t -> t -> bool

val of_int : int -> t
val to_int : t -> int

val of_float : float -> t
val to_float : t -> float

val of_string : string -> t
val to_string : t -> string

val vec_get  : t -> int -> t
val vec_set  : t -> int -> t -> unit
val vec_size : t -> int

val initialize_module : unit
