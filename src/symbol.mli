(** An Emacs symbol, i.e., an Emacs value that satisfies [Value.is_symbol].

    [(Info-goto-node "(elisp)Symbols")] *)

open! Core_kernel
open! Import0

include Symbol0_intf.Symbol0 with type t = Symbol0.t

(** Accessors *)
val name         : t -> string         (** [(describe-function 'symbol-name)    ] *)
val function_exn : t -> Value.t        (** [(describe-function 'symbol-function)] *)

val compare_name : t -> t -> int

(** [(describe-function 'make-symbol)]. *)
val create : name : string -> t

(** [(describe-function 'gensym)]. *)
val gensym : ?prefix : string -> unit -> t

(** [(describe-function 'fset)]. *)
val set_function : t -> Value.t -> unit

type symbol = t

module Property : sig
  type 'a t
  [@@deriving sexp_of]

  (** A property of an Emacs symbol.

      [(Info-goto-node "(elisp)Symbol Properties")] *)

  (** Create a property named by the given symbol. *)
  val create : symbol -> 'a Value.Type.t -> 'a t

  (** [(describe-function 'get)] *)
  val get : 'a t -> symbol -> 'a option

  (** [(describe-function 'get)] *)
  val get_exn : 'a t -> symbol -> 'a

  (** [(describe-function 'put)] *)
  val put : 'a t -> symbol -> 'a -> unit

end

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
