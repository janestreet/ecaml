(** An Emacs symbol, i.e., an Emacs value that satisfies [Value.is_symbol].

    [(Info-goto-node "(elisp)Symbols")] *)

open! Core_kernel
open! Import

include Value.Subtype

include Equal.S       with type t := t
include Value.Funcall with type t := t

(** Accessors *)
val name         : t -> string         (** [(describe-function 'symbol-name)    ] *)
val function_exn : t -> Value.t        (** [(describe-function 'symbol-function)] *)

val compare_name : t -> t -> int

(** [(describe-function 'intern)]. *)
val intern : string -> t

(** [(describe-function 'make-symbol)]. *)
val create : name : string -> t

(** [(describe-function 'fset)]. *)
val set_function : t -> Value.t -> unit

type symbol = t

module type Subtype = sig
  type t

  val of_symbol_exn : symbol -> t
  val to_symbol     : t      -> symbol

  val of_value_exn : Value.t -> t
  val to_value     : t       -> Value.t
end

module Make_subtype (Arg : sig
    type t [@@deriving enumerate, sexp_of]
    val module_name : string
    val to_symbol : t -> symbol
  end) : Subtype with type t := Arg.t
