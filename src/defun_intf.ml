open! Core_kernel
open! Import

(** [Defun] is an applicative binding of Elisp [defun].

    It does for Elisp what [Command] does for the command line. *)

module type S = sig
  type 'a t

  val return : 'a -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val both : 'a t -> 'b t -> ('a * 'b) t

  val required : Symbol.t -> 'a Value.Type.t -> 'a t

  val optional : Symbol.t -> 'a Value.Type.t -> 'a option t

  val rest : Symbol.t -> 'a Value.Type.t -> 'a list t

  val optional_with_default : Symbol.t -> 'a -> 'a Value.Type.t -> 'a t
end

module type Defun = sig
  type 'a t

  module Open_on_rhs_intf : sig
    module type S = S with type 'a t = 'a t
  end

  include Applicative.Let_syntax with type 'a t := 'a t
    with module Open_on_rhs_intf := Open_on_rhs_intf

  include Open_on_rhs_intf.S with type 'a t := 'a t

  val defun
    :  ?define_keys:(Keymap.t * string) list
    -> ?docstring:string
    -> ?interactive:string
    -> Source_code_position.t
    -> 'a Value.Type.t
    -> Symbol.t
    -> 'a t
    -> unit

  val defun_nullary
    :  ?define_keys:(Keymap.t * string) list
    -> ?docstring:string
    -> ?interactive:string
    -> Source_code_position.t
    -> 'a Value.Type.t
    -> Symbol.t
    -> (unit -> 'a)
    -> unit

  val defun_nullary_nil
    :  ?define_keys:(Keymap.t * string) list
    -> ?docstring:string
    -> ?interactive:string
    -> Source_code_position.t
    -> Symbol.t
    -> (unit -> unit)
    -> unit

  val lambda
    :  ?docstring:string
    -> ?interactive:string
    -> Source_code_position.t
    -> 'a Value.Type.t
    -> 'a t
    -> Function.t

  val lambda_nullary
    :  ?docstring:string
    -> ?interactive:string
    -> Source_code_position.t
    -> 'a Value.Type.t
    -> (unit -> 'a)
    -> Function.t

  val lambda_nullary_nil
    :  ?docstring:string
    -> ?interactive:string
    -> Source_code_position.t
    -> (unit -> unit)
    -> Function.t
end
