(** A buffer-local is a variable that can have a different value associated with each
    buffer.

    [Buffer_local] is like [Var], with the additional guarantee that the variable is
    buffer local.

    [(Info-goto-node "(elisp)Buffer-Local Variables")]. *)

open! Core
open! Import

module type Defvar_embedded_arg = sig
  type t [@@deriving sexp_of]
end

module type Buffer_local = sig
  type 'a t [@@deriving sexp_of]

  val symbol : _ t -> Symbol.t
  val var : 'a t -> 'a Var.t

  (** [defvar] defines a buffer-local variable using [Defvar.defvar], and calls
      [Var.make_buffer_local_always]. *)
  val defvar
    :  Symbol.t
    -> Source_code_position.t
    -> ?docstring:string
    -> type_:'a Value.Type.t
    -> default_value:'a
    -> unit
    -> 'a t

  (** [wrap_existing var] takes a variable defined in Elisp and makes it available as a
      [Buffer_local.t].  If [make_buffer_local_always = false], then [wrap_existing]
      raises if [Var.is_buffer_local_always var = false].  If [make_buffer_local_always =
      true], then [wrap_existing] calls [Var.make_buffer_local_always var]. *)
  val wrap_existing
    :  ?make_buffer_local_always:bool (** default is [false] *)
    -> Symbol.t
    -> 'a Value.Type.t
    -> 'a t

  (** [defvar_embedded] defines a buffer-local variable whose Elisp representation is an
      opaque pointer to an OCaml value, via [Value.Type.caml_embed].  This allows one to
      store an arbitrary OCaml value in a buffer local, without any conversions between
      OCaml and Elisp. *)
  val defvar_embedded
    :  Symbol.t
    -> Source_code_position.t
    -> ?docstring:string
    -> (module Defvar_embedded_arg with type t = 'a)
    -> 'a option t

  val get : 'a t -> Buffer.t -> 'a
  val get_exn : 'a option t -> Buffer.t -> 'a
  val set : 'a t -> 'a -> Buffer.t -> unit

  module Private : sig
    val get_in_current_buffer : 'a t -> 'a
    val get_in_current_buffer_exn : 'a option t -> 'a
    val set_in_current_buffer : 'a t -> 'a -> unit
  end
end
