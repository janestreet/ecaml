(** An Emacs symbol, i.e., an Emacs value that satisfies [Value.is_symbol].

    [(Info-goto-node "(elisp)Symbols")] *)

open! Core_kernel
open! Import

type t [@@deriving sexp_of]

include Equal.S       with type t := t
include Value.Funcall with type t := t
include Value.Subtype with type t := t

(** Accessors *)
val name         : t -> string         (** [(describe-function 'symbol-name)    ] *)
val function_exn : t -> Value.t        (** [(describe-function 'symbol-function)] *)
val value        : t -> Value.t option (** [(describe-function 'symbol-value)   ] *)
val value_exn    : t -> Value.t        (** [(describe-function 'symbol-value)   ] *)

(** [(describe-function 'intern)]. *)
val intern : string -> t

(** [(describe-function 'make-symbol)]. *)
val create : name : string -> t

(** [(describe-function 'fset)]. *)
val set_function : t -> Value.t -> unit

(** [(describe-function 'set)]. *)
val set_value : t -> Value.t -> unit

(** [(describe-function 'makunbound)] *)
val clear_value : t -> unit

val set_value_temporarily : t -> Value.t -> f:(unit -> 'a) -> 'a

val set_values_temporarily : (t * Value.t) list -> f:(unit -> 'a) -> 'a

(** [(describe-function 'bound-and-true-p]. *)
val has_non_null_value : t -> bool

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
