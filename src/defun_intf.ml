open! Core_kernel
open! Import

(** [Defun] is an applicative binding of Elisp [defun].

    It does for Elisp what [Command] does for the command line. *)

module type Open_on_rhs = sig
  type 'a t

  val return : 'a -> 'a t
  val map    : 'a t -> f:('a -> 'b) -> 'b t
  val both   : 'a t -> 'b t -> ('a * 'b) t

  val required : Symbol.t -> 'a Value.Type.t -> 'a        t
  val optional : Symbol.t -> 'a Value.Type.t -> 'a option t
  val rest     : Symbol.t -> 'a Value.Type.t -> 'a list   t

  val optional_with_default : Symbol.t -> 'a -> 'a Value.Type.t -> 'a t
end

module type Defun = sig

  type 'a t

  include Open_on_rhs with type 'a t := 'a t

  module Let_syntax : sig
    module Let_syntax : sig
      type 'a t (** substituted below *)
      include Open_on_rhs with type 'a t := 'a t
      module Open_on_rhs : Open_on_rhs with type 'a t := 'a t
    end with type 'a t := 'a t
  end

  val defun
    :  ?docstring   : string
    -> ?interactive : string
    -> Source_code_position.t
    -> 'a Value.Type.t
    -> Symbol.t
    -> 'a t
    -> unit

  val defun_nullary
    :  ?docstring : string
    -> ?interactive : string
    -> Source_code_position.t
    -> 'a Value.Type.t
    -> Symbol.t
    -> (unit -> 'a)
    -> unit

  val defun_nullary_nil
    :  ?docstring : string
    -> ?interactive : string
    -> Source_code_position.t
    -> Symbol.t
    -> (unit -> unit)
    -> unit

  val lambda
    :  ?docstring : string
    -> ?interactive : string
    -> Source_code_position.t
    -> 'a Value.Type.t
    -> 'a t
    -> Function.t

  val lambda_nullary
    :  ?docstring : string
    -> ?interactive : string
    -> Source_code_position.t
    -> 'a Value.Type.t
    -> (unit -> 'a)
    -> Function.t

  val lambda_nullary_nil
    :  ?docstring : string
    -> ?interactive : string
    -> Source_code_position.t
    -> (unit -> unit)
    -> Function.t
end
