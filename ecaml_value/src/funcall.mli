(** A typeful interface for calling Elisp, as [external] does for C.  *)

open! Core_kernel
open! Import

type 'a t

(** [Wrap] wraps an Elisp function as an OCaml function.  Idiomatic use looks like:

    {[
      let not = Funcall.Wrap.("not" <: bool @-> return bool)
      let about_emacs = Funcall.Wrap.("about-emacs" <: nullary @-> return nil)
    ]} *)
module Wrap : sig
  val return : 'a Value.Type.t -> 'a t
  val nil : unit Value.Type.t
  val nullary : unit Value.Type.t
  val ( <: ) : string -> 'a t -> 'a
  val ( @-> ) : 'a Value.Type.t -> 'b t -> ('a -> 'b) t

  include Value.Type.S
end

module Private : sig
  val apply : 'a t -> 'a -> Value.t list -> on_parse_error:(exn -> Value.t) -> Value.t
  val wrap_unrolled : 'a t -> Value.t -> 'a
end
