(** The "advice" feature lets you add to the existing definition of a function, by
    wrapping around the original function a "piece of advice".

    Each piece of advice receives the original function and arguments; its result is
    returned to the caller as if from the original function.

    A function may have multiple pieces of advice, which are composed by nesting earlier
    advice within later advice.

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
  -> ?docstring:string
  -> for_function:Symbol.t
  -> ?interactive:Defun.Interactive.t
  -> ((Value.t list -> Value.t) -> Value.t list -> Value.t)
  -> unit

(** [around_funcall] provides typeful access to the arguments and return value of
    [for_function]. *)
val around_funcall
  :  Symbol.t
  -> Source_code_position.t
  -> ?docstring:string
  -> for_function:Symbol.t
  -> ?interactive:Defun.Interactive.t
  -> 'a Funcall.t
  -> ('a -> 'a)
  -> unit

(** [(describe-function 'advice-add)] *)
val add_predefined_function : Symbol.t -> for_function:Symbol.t -> unit

(** [(describe-function 'advice-remove)] *)
val remove : Symbol.t -> for_function:Symbol.t -> unit
