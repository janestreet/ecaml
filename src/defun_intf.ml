(** [Defun] is an applicative binding of Elisp [defun].

    It does for Elisp what [Command] does for the command line. *)

open! Core
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

  module For_testing : sig
    val all_defun_symbols : unit -> Symbol.t list
  end

  (** An [Returns.t] states the return type of a function and whether the function returns
      a value of that type directly or via a [Deferred.t]. An [(a, a) Returns.t] means
      that the function returns [a] directly. An [(a, a Deferred.t) Returns.t] means that
      the function returns [a] via an [a Deferred.t]. *)
  module Returns : sig
    type (_, _) t =
      | Returns : 'a Value.Type.t -> ('a, 'a) t
      | Returns_deferred : 'a Value.Type.t -> ('a, 'a Deferred.t) t
    [@@deriving sexp_of]

    val return : ('a, 'b) t -> 'a -> 'b
    val returns : ('a, 'b) Sync_or_async.t -> 'a Value.Type.t -> ('a, 'b) t
  end

  module Interactive : sig
    (** [(describe-function 'interactive)] *)
    type t =
      | Args of Function.t
      | Form of Form.t
      | Function_name of { prompt : string }
      (** a -- Function name: symbol with a function definition. *)
      | Ignored (** i -- Ignored, i.e. always nil. Does not do I/O. *)
      | No_arg (** interactive with no argument spec *)
      | Prompt of string
      (** s -- Any string. Does not inherit the current input method. *)
      | Raw_prefix (** P -- Prefix arg in raw form. Does not do I/O. *)
      | Prefix (** p -- Prefix arg converted to number. Does not do I/O. *)
      | Region
      (** r -- Region: point and mark as 2 numeric args, smallest first. Does no I/O. *)

    (** An interactive form which evaluates to a list of constant values. *)
    val list : Value.t list -> t
  end

  type 'a defun :=
    Symbol.t
    -> Source_code_position.t
    -> docstring:string
    -> ?define_keys:(Keymap.t Var.t * string) list
    -> ?obsoletes:Obsoletes.t
    -> ?should_profile:bool
    -> ?interactive:Interactive.t
    -> 'a

  val defun : ((_, 'a) Returns.t -> 'a t -> unit) defun
  val defun_nullary : ((_, 'a) Returns.t -> (unit -> 'a) -> unit) defun
  val defun_nullary_nil : ((unit -> unit) -> unit) defun
  val defun_func : ((_, 'a) Returns.t -> 'a t -> Function.t) defun

  (** Define a function which will be used in another function's interactive spec.

      When a command defined with [~interactive:(defun_interactive_arg ... f)] is called
      interactively, [f ()] is called to compute the argument values to supply to the
      command. Of course, the argument values should match the command's [Defun.t]
      specification. *)
  val defun_interactive_arg
    :  Symbol.t
    -> here:[%call_pos]
    -> (unit -> Value.t list Deferred.t)
    -> Interactive.t

  (** [(describe-function 'defalias)] [(Info-goto-node "(elisp)Defining Functions")] *)
  val defalias
    :  Symbol.t
    -> here:[%call_pos]
    -> ?docstring:string
    -> alias_of:Value.t
    -> unit
    -> unit

  (** [(describe-function 'define-obsolete-function-alias)]

      N.B. Load order matters. A subsequent [defun] will override the aliasing. *)
  val define_obsolete_alias
    :  Symbol.t
    -> here:[%call_pos]
    -> ?docstring:string
    -> alias_of:Symbol.t
    -> since:string
    -> unit
    -> unit

  val lambda : Source_code_position.t -> (_, 'a) Returns.t -> 'a t -> Function.t

  val lambda_nullary
    :  Source_code_position.t
    -> (_, 'a) Returns.t
    -> (unit -> 'a)
    -> Function.t

  val lambda_nullary_nil : Source_code_position.t -> (unit -> unit) -> Function.t

  (** [apply t args ~function_ ~defined_at] applies [args] to [t], using [function_] and
      [defined_at] to construct a nice error message if [args] are not what [t] expects.

      It helps define functions which must inspect some of their arguments to determine
      the number and type of the remaining arguments. *)
  val apply
    :  'a t
    -> Value.t array
    -> function_:Sexp.t
    -> defined_at:Lexing.position
    -> 'a
end
