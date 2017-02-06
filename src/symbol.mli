(** An Emacs symbol, i.e., an Emacs value that satisfies [Value.is_symbol]. *)

open! Core
open! Async
open! Import

type t [@@deriving sexp_of]

val of_value_exn : Value.t -> t
val to_value     : t -> Value.t

val intern : string -> t

val name : t -> string

val funcall   : t -> Value.t list -> Value.t
val funcall_i : t -> Value.t list -> unit

val fset : t -> Value.t -> unit
val set  : t -> Value.t -> unit

val provide : t -> unit
val require : t -> unit

val apply       : t
val cons        : t
val eval        : t
val format      : t
val interactive : t
val lambda      : t
val list        : t
val nil         : t
val progn       : t
val quote       : t
val read        : t
val t           : t
