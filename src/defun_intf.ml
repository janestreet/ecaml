(** [Defun] is an applicative binding of Elisp [defun].

    It does for Elisp what [Command] does for the command line. *)

open! Core_kernel
open! Async_kernel
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
  type 'a t [@@deriving sexp_of]

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

  val defun_raw
    :  Symbol.t
    -> Source_code_position.t
    -> ?docstring:string
    -> ?interactive:string
    -> args:Symbol.t list
    -> ?optional_args:Symbol.t list
    -> ?rest_arg:Symbol.t
    -> Function.Fn.t
    -> unit

  module Returns : sig
    type 'a t =
      | Returns of 'a Value.Type.t
      (** [Returns_unit_deferred] is meant to be used for interactive commands directly
          called by the user.  It is restricted to functions returning [unit Deferred.t]
          to encourage this use case.  Calling such a function from within async raises.
          Restricting [Returns_unit_deferred] to top-level interactive commands makes it
          less likely that this situation will occur. *)
      | Returns_unit_deferred : unit Deferred.t t
    [@@deriving sexp_of]
  end

  val defun
    :  Symbol.t
    -> Source_code_position.t
    -> ?docstring:string
    -> ?define_keys:(Keymap.t * string) list
    -> ?obsoletes:Symbol.t
    -> ?interactive:Interactive.t
    -> 'a Returns.t
    -> 'a t
    -> unit

  (** [(describe-function 'defalias)]
      [(Info-goto-node "(elisp)Defining Functions")] *)
  val defalias
    :  Symbol.t
    -> Source_code_position.t
    -> ?docstring:string
    -> alias_of:Symbol.t
    -> unit
    -> unit

  (** [(describe-function 'define-obsolete-function-alias)]

      N.B. Load order matters.  A subsequent [defun] will override the aliasing. *)
  val define_obsolete_alias
    :  Symbol.t
    -> Source_code_position.t
    -> ?docstring:string
    -> alias_of:Symbol.t
    -> since:string
    -> unit
    -> unit

  val defun_nullary
    :  Symbol.t
    -> Source_code_position.t
    -> ?docstring:string
    -> ?define_keys:(Keymap.t * string) list
    -> ?obsoletes:Symbol.t
    -> ?interactive:Interactive.t
    -> 'a Returns.t
    -> (unit -> 'a)
    -> unit

  val defun_nullary_nil
    :  Symbol.t
    -> Source_code_position.t
    -> ?docstring:string
    -> ?define_keys:(Keymap.t * string) list
    -> ?obsoletes:Symbol.t
    -> ?interactive:Interactive.t
    -> (unit -> unit)
    -> unit

  val lambda
    :  Source_code_position.t
    -> ?docstring:string
    -> ?interactive:Interactive.t
    -> 'a Returns.t
    -> 'a t
    -> Function.t

  val lambda_nullary
    :  Source_code_position.t
    -> ?docstring:string
    -> ?interactive:Interactive.t
    -> 'a Returns.t
    -> (unit -> 'a)
    -> Function.t

  val lambda_nullary_nil
    :  Source_code_position.t
    -> ?docstring:string
    -> ?interactive:Interactive.t
    -> (unit -> unit)
    -> Function.t

  module Private : sig
    val block_on_async :
      (Source_code_position.t
       -> ?context:Sexp.t Lazy.t
       -> (unit -> unit Deferred.t)
       -> unit)
        Set_once.t
  end
end
