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

open! Core
open! Import

type t [@@deriving sexp_of]

val of_function : Symbol.t -> t

(** [defun_around_values advice_name here ~docstring f] creates advice named [advice_name]
    with body [f]. Compared to [defun_around_funcall], [defun_around_values] provides
    looser coupling when the advice doesn't need to interact with the arguments or return
    value of the advised function. *)
val defun_around_values
  :  Symbol.t
  -> here:[%call_pos]
  -> (Value.t, 'a) Sync_or_async.t
  -> docstring:string
  -> ?interactive:Defun.Interactive.t
  -> ?should_profile:bool
  -> ((Value.t list -> Value.t) -> Value.t list -> 'a)
  -> t

module On_parse_error : sig
  type t =
    | Allow_raise
    | Call_inner_function
  [@@deriving sexp_of]
end

(** [defun_around_funcall] provides typeful access to the arguments and return value of
    the advised function. *)
val defun_around_funcall
  :  Symbol.t
  -> here:[%call_pos]
  -> docstring:string
  -> ?interactive:Defun.Interactive.t
  -> ?on_parse_error:On_parse_error.t
  -> ?should_profile:bool
  -> 'a Funcall.t
  -> ('a -> 'a)
  -> t

(** [(describe-function 'advice-add)] *)
val add : t -> to_function:Symbol.t -> unit

(** [(describe-function 'advice-remove)] *)
val remove : t -> from_function:Symbol.t -> unit
