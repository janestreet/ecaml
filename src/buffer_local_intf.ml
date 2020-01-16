(** A buffer-local is a variable that can have a different value associated with each
    buffer.

    [Buffer_local] is like [Var], with the additional guarantee that the variable is
    buffer local.

    Where a module defines a type [t] and a value of type [t Buffer_local.t], the
    conventional name for that value is [in_buffer].

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

  (** Idiomatic usage of [Wrap] looks like:

      {[ Buffer_local.Wrap.(SYMBOL_NAME <: TYPE) ]}

      For example:

      {[ Buffer_local.Wrap.("default-directory" <: string) ]}

      To use [~make_buffer_local_always:true], the idiom is:

      {[
        Buffer_local.Wrap.(
          let ( <: ) = ( <: ) ~make_buffer_local_always:true in
          SYMBOL_NAME <: TYPE)
      ]} *)
  module Wrap : sig
    val ( <: )
      :  ?make_buffer_local_always:bool (** default is [false] *)
      -> string
      -> 'a Value.Type.t
      -> 'a t

    include Value.Type.S
  end

  (** [defvar_embedded] defines a buffer-local variable whose Elisp representation is an
      opaque pointer to an OCaml value, via [Caml_embed.create_type].  This allows one to
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
  val update_exn : 'a option t -> Buffer.t -> f:('a -> 'a) -> unit

  (** A permanent buffer-local variable is unaffected by [kill-all-local-variables], and
      so it is not cleared by changing major modes.
      See [(Info-goto-node "(elisp)Creating Buffer-Local")]. *)
  val set_permanent : _ t -> bool -> unit

  val is_permanent : _ t -> bool

  module Private : sig
    val get_in_current_buffer : 'a t -> 'a
    val get_in_current_buffer_exn : 'a option t -> 'a
    val set_in_current_buffer : 'a t -> 'a -> unit

    val set_temporarily_in_current_buffer
      :  (_, 'a) Sync_or_async.t
      -> 'b t
      -> 'b
      -> f:(unit -> 'a)
      -> 'a
  end
end
