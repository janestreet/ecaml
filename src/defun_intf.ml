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
  val required : string -> 'a Value.Type.t -> 'a t
  val optional : string -> 'a Value.Type.t -> 'a option t
  val rest : string -> 'a Value.Type.t -> 'a list t
  val optional_with_default : string -> 'a -> 'a Value.Type.t -> 'a t

  (** An optional argument whose [Value.Type.t] handles [nil] directly. *)
  val optional_with_nil : string -> 'a Value.Type.t -> 'a t

  include Value.Type.S
end

module Obsoletes = struct
  module Since = struct
    type t = Since of string [@@deriving sexp_of]
  end

  type t = Symbol.t * Since.t [@@deriving sexp_of]
end

module type Defun = sig
  module Obsoletes = Obsoletes

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
      | Args of (unit -> Value.t list Deferred.t)
      (** When a command defined with [~interactive:(Args f)] is called interactively, [f
          ()] is called to compute the argument values to supply to the command.  Of
          course, the argument values should match the command's [Defun.t]
          specification. *)
      | Function_name of { prompt : string }
      | Ignored
      | No_arg
      | Prompt of string
      | Raw_prefix
      | Region

    include Valueable.S with type t := t
  end

  module For_testing : sig
    val all_defun_symbols : unit -> Symbol.t list
  end

  (** An [Returns.t] states the return type of a function and whether the function returns
      a value of that type directly or via a [Deferred.t].  An [(a, a) Returns.t] means
      that the function returns [a] directly.  An [(a, a Deferred.t) Returns.t] means that
      the function returns [a] via an [a Deferred.t]. *)
  module Returns : sig
    type (_, _) t =
      | Returns : 'a Value.Type.t -> ('a, 'a) t
      | Returns_deferred : 'a Value.Type.t -> ('a, 'a Deferred.t) t
    [@@deriving sexp_of]

    val returns : ('a, 'b) Sync_or_async.t -> 'a Value.Type.t -> ('a, 'b) t
  end

  type 'a defun :=
    Symbol.t
    -> Source_code_position.t
    -> ?docstring:string
    -> ?define_keys:(Keymap.t * string) list
    -> ?obsoletes:Obsoletes.t
    -> ?should_profile:bool
    -> ?interactive:Interactive.t
    -> ?disabled:bool (** See {!Symbol.Property.function_disabled}  *)
    -> ?evil_config:Evil.Config.t
    -> 'a

  val defun : ((_, 'a) Returns.t -> 'a t -> unit) defun
  val defun_nullary : ((_, 'a) Returns.t -> (unit -> 'a) -> unit) defun
  val defun_nullary_nil : ((unit -> unit) -> unit) defun

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

  val lambda
    :  Source_code_position.t
    -> ?docstring:string
    -> ?interactive:Interactive.t
    -> (_, 'a) Returns.t
    -> 'a t
    -> Function.t

  val lambda_nullary
    :  Source_code_position.t
    -> ?docstring:string
    -> ?interactive:Interactive.t
    -> (_, 'a) Returns.t
    -> (unit -> 'a)
    -> Function.t

  val lambda_nullary_nil
    :  Source_code_position.t
    -> ?docstring:string
    -> ?interactive:Interactive.t
    -> (unit -> unit)
    -> Function.t
end
