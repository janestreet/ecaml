open! Core
open! Import
include module type of Sexp
include Valueable.S with type t := t

module Private : sig
  (** Parse the output of Elisp's [prin1] into a sexp. Exposed for testing. *)
  val of_prin1_output : string -> t
end
