(** The "advice" feature lets you add to the existing definition of a function, by
    wrapping around the original function a "piece of advice".

    Each piece of advice receives the original function and arguments; its result is
    returned to the caller as if from the original function.

    A function may have multiple pieces of advice, which are composed by nesting earlier
    advice within later advice.

    [(Info-goto-node "(elisp)Advising Functions")] *)

open! Core_kernel
open! Import

(** [add ?docstring ?interactive here return_type advice_name f ~for_function ]
    defines function [advice_name] with body [f] and adds it as advice on [for_function].
*)
val add
  :  ?docstring   : string
  -> ?interactive : string
  -> Source_code_position.t
  -> 'a Value.Type.t
  -> Symbol.t
  -> (inner : (Value.t list -> Value.t) -> inner_args : Value.t list -> 'a)
  -> for_function : Symbol.t
  -> unit

(** [(describe-function 'advice-add)] *)
val add_predefined_function
  :  Symbol.t
  -> for_function : Symbol.t
  -> unit

(** [(describe-function 'advice-remove)] *)
val remove
  :  Symbol.t
  -> for_function : Symbol.t
  -> unit
