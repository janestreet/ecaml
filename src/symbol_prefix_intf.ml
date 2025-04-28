(** Manages the prefixes used in lieu of namespacing among Elisp symbols. *)

open! Core
open! Import0

module type S = sig
  type t

  val symbol_prefix : t

  (** Name of this module. *)
  val elisp_name : string

  (** All commands for which [symbol_prefix] is a prefix.

      N.B. This includes symbols interned with extensions of [symbol_prefix]. *)
  val all_commands : Symbol.t list Lazy.t

  (** Interns a new symbol prefixed with [symbol_prefix].

      It is an error to call [symbol] after forcing [all_commands]. *)
  val symbol : string -> Symbol.t

  (** [prefixed_symbol_name s] is the name of the symbol that [symbol s] would intern. *)
  val prefixed_symbol_name : string -> string
end

module type Symbol_prefix = sig
  type t [@@deriving sexp_of]

  module type S = S with type t := t

  val create : string -> (module S)
  val extend : t -> string -> (module S)
end
