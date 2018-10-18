(** [Defun] is an applicative binding of Elisp [defun].

    It does for Elisp what [Command] does for the command line. *)

open! Core_kernel
open! Import

module type S = sig
  type 'a t

  val return : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val required : Symbol.t -> 'a Value.Type.t -> 'a t
  val optional : Symbol.t -> 'a Value.Type.t -> 'a option t
  val rest : Symbol.t -> 'a Value.Type.t -> 'a list t
  val optional_with_default : Symbol.t -> 'a -> 'a Value.Type.t -> 'a t

  (** An optional argument whose [Value.Type.t] handles [nil] directly. *)
  val optional_with_nil : Symbol.t -> 'a Value.Type.t -> 'a t

  include Value.Type.S
end

module type Defun = sig
  type 'a t

  module Open_on_rhs_intf : sig
    module type S = S with type 'a t = 'a t
  end

  include
    Applicative.Let_syntax
    with type 'a t := 'a t
    with module Open_on_rhs_intf := Open_on_rhs_intf

  include Open_on_rhs_intf.S with type 'a t := 'a t

  module Interactive : sig
    type t =
      | No_arg
      | Ignored
      | Prompt of string
      | Raw_prefix
      | Region

    include Valueable.S with type t := t
  end

  module For_testing : sig
    val defun_symbols : Symbol.t list ref
  end

  val defun_raw : (Symbol.t -> Function.Fn.t -> unit) Function.with_spec

  val defun
    :  ?define_keys:(Keymap.t * string) list
    -> ?docstring:string
    -> ?interactive:Interactive.t
    -> ?obsoletes:Symbol.t
    -> Source_code_position.t
    -> returns:'a Value.Type.t
    -> Symbol.t
    -> 'a t
    -> unit

  val defun_nullary
    :  ?define_keys:(Keymap.t * string) list
    -> ?docstring:string
    -> ?interactive:Interactive.t
    -> ?obsoletes:Symbol.t
    -> Source_code_position.t
    -> returns:'a Value.Type.t
    -> Symbol.t
    -> (unit -> 'a)
    -> unit

  val defun_nullary_nil
    :  ?define_keys:(Keymap.t * string) list
    -> ?docstring:string
    -> ?interactive:Interactive.t
    -> ?obsoletes:Symbol.t
    -> Source_code_position.t
    -> Symbol.t
    -> (unit -> unit)
    -> unit

  (** [defun_blocking_async] is meant to be used for interactive commands directly called
      by the user. It is restricted to functions returning [unit Async.Deferred.t] to
      encourage this use case.

      You cannot call a defun_blocking_async function from within async. Doing so results
      in a runtime exception. Restricting [defun_blocking_async] to top-level interactive
      commands makes it less likely that this situation will occur. *)
  val defun_blocking_async
    :  ?define_keys:(Keymap.t * string) list
    -> ?docstring:string
    -> ?interactive:Interactive.t
    -> ?obsoletes:Symbol.t
    -> ?timeout:Time.Span.t option
    -> Source_code_position.t
    -> Symbol.t
    -> unit Async.Deferred.t t
    -> unit

  val lambda
    :  ?docstring:string
    -> ?interactive:Interactive.t
    -> Source_code_position.t
    -> returns:'a Value.Type.t
    -> 'a t
    -> Function.t

  val lambda_nullary
    :  ?docstring:string
    -> ?interactive:Interactive.t
    -> Source_code_position.t
    -> returns:'a Value.Type.t
    -> (unit -> 'a)
    -> Function.t

  val lambda_nullary_nil
    :  ?docstring:string
    -> ?interactive:Interactive.t
    -> Source_code_position.t
    -> (unit -> unit)
    -> Function.t
end
