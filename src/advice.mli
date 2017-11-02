(** The "advice" feature lets you add to the existing definition of a function, by
    "advising the function".  Each function can have multiple "pieces of advice",
    separately defined.  Each defined piece of advice can be "enabled" or "disabled"
    explicitly.  All the enabled pieces of advice for any given function actually take
    effect when you "activate" advice for that function, or when you define or redefine
    the function.  Note that enabling a piece of advice and activating advice for a
    function are not the same thing.

    [(Info-goto-node "(elisp)Advising Functions")] *)

open! Core_kernel
open! Import

(** A [Class.t] specifies the "class" of the advice--one of `before', `after',
    or `around'.  Before-advice runs before the function itself;
    after-advice runs after the function itself; around-advice is wrapped
    around the execution of the function itself.
    [(Info-goto-node "(elisp)Defining Advice")] *)
module Class : sig
  type t =
    | After
    | Around
    | Before
  [@@deriving sexp_of]
end

(** The position of one piece of advice in the list of all pieces of advice for a
    function. *)
module Position : sig
  type t =
    | First
    | Last
    | Zero_based of int
  [@@deriving sexp_of]
end

module Name : sig
  type t [@@deriving sexp_of]

  val of_symbol : Symbol.t -> t
end

(** [defadvice] currently only supports [Around] advice.
    [(describe-function 'defadvice)]
    [(Info-goto-node "(elisp)Defining Advice")]
    [(Info-goto-node "(elisp)Around-Advice")] *)
val defadvice
  :  ?docstring : string
  -> ?position  : Position.t  (** default is [First] *)
  -> Source_code_position.t
  -> advice_name  : Name.t
  -> for_function : Symbol.t
  -> (args : Value.t list
      -> inner : (Value.t list -> Value.t)
      -> Value.t)
  -> unit

(** [(describe-function 'ad-enable-advice)]
    [(Info-goto-node "(elisp)Enabling Advice")] *)
val enable
  :  ?class_ : Class.t  (** default is [Around] *)
  -> Name.t
  -> Symbol.t
  -> unit

(** [(describe-function 'ad-disable-advice)]
    [(Info-goto-node "(elisp)Enabling Advice")] *)
val disable
  :  ?class_ : Class.t  (** default is [Around] *)
  -> Name.t
  -> Symbol.t
  -> unit

(** [(describe-function 'ad-activate)]
    [(Info-goto-node "(elisp)Activation of advice")] *)
val activate_function : Symbol.t -> unit

(** [(describe-function 'ad-activate-regexp)]
    [(Info-goto-node "(elisp)Activation of advice")] *)
val activate_functions_with_advice_matching : Regexp.t -> unit

(** [(describe-function 'ad-activate-all)]
    [(Info-goto-node "(elisp)Activation of advice")] *)
val activate_all : unit -> unit

(** [(describe-function 'ad-deactivate)]
    [(Info-goto-node "(elisp)Activation of advice")] *)
val deactivate_function : Symbol.t -> unit

(** [(describe-function 'ad-deactivate-regexp)]
    [(Info-goto-node "(elisp)Activation of advice")] *)
val deactivate_functions_with_advice_matching : Regexp.t -> unit

(** [(describe-function 'ad-deactivate-all)]
    [(Info-goto-node "(elisp)Activation of advice")] *)
val deactivate_all : unit -> unit

(** [(describe-function 'ad-unadvise)]
    [(Info-goto-node "(elisp)Defining advice")] *)
val unadvise_function : Symbol.t -> unit

(** [(describe-function 'ad-unadvise-all)]
    [(Info-goto-node "(elisp)Defining advice")] *)
val unadvise_all : unit -> unit
