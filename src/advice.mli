(** The "advice" feature lets you add to the existing definition of a function, by
    wrapping around the original function a "piece of advice".

    Each piece of advice receives the original function and arguments; its result is
    returned to the caller as if from the original function.

    A function may have multiple pieces of advice, which are composed by nesting earlier
    advice within later advice.

    If async advice is added to an elisp function, that function must be called with
    [run_outside_async] or it will raise at runtime. This cannot be detected at compile
    time because one can add advice at runtime.

    [(Info-goto-node "(elisp)Advising Functions")] *)

open! Core_kernel
open! Import

(** [around_values ?docstring ?interactive here return_type advice_name f ~for_function ]
    defines function [advice_name] with body [f] and adds it as advice on [for_function].

    Compared to [around_funcall], [around_values] provides looser coupling when the advice
    doesn't need to interact with the arguments or return value of [for_function].
*)
val around_values
  :  Symbol.t
  -> Source_code_position.t
  -> (Value.t, 'a) Sync_or_async.t
  -> ?docstring:string
  -> for_function:Symbol.t
  -> ?interactive:Defun.Interactive.t
  -> ?should_profile:bool
  -> ((Value.t list -> Value.t) -> Value.t list -> 'a)
  -> unit

module On_parse_error : sig
  type t =
    | Allow_raise
    | Call_inner_function
  [@@deriving sexp_of]
end

(** [around_funcall] provides typeful access to the arguments and return value of
    [for_function]. *)
val around_funcall
  :  Symbol.t
  -> Source_code_position.t
  -> ?docstring:string
  -> for_function:Symbol.t
  -> ?interactive:Defun.Interactive.t
  -> ?on_parse_error:On_parse_error.t
  -> ?should_profile:bool
  -> 'a Funcall.t
  -> ('a -> 'a)
  -> unit

(** [(describe-function 'advice-add)] *)
val add_predefined_function : Symbol.t -> for_function:Symbol.t -> unit

(** [(describe-function 'advice-remove)] *)
val remove : Symbol.t -> for_function:Symbol.t -> unit
