(** A typeful interface for calling Elisp, as [external] does for C. E.g., {[

      let not : bool -> bool = "not" <: bool @-> return bool

      let about_emacs : unit -> unit = "about-emacs" <: nullary @-> return nil

    ]} *)

open! Core_kernel
open! Import

type 'a t

val return : 'a Value.Type.t -> 'a t
val nil : unit Value.Type.t
val nullary : unit Value.Type.t

(** ["foo" <: ty] imports Elisp function whose name is "foo" and whose type is [ty], like
    how [foo :> ty] types an OCaml value. *)
val ( <: ) : string -> 'a t -> 'a

val ( @-> ) : 'a Value.Type.t -> 'b t -> ('a -> 'b) t

include Value.Type.S

module Private : sig
  val apply : 'a t -> 'a -> Value.t list -> on_parse_error:(exn -> Value.t) -> Value.t
  val wrap_unrolled : 'a t -> Value.t -> 'a
end
