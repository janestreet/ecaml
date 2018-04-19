(** [(Info-goto-node "(elisp)Input Events")] *)

open! Core_kernel
open! Import

include Input_event0_intf.Input_event0_public with type t = Input_event0.t

(** [create_exn string] is a specialized version of [Key_sequence.create] that raises
    unless [string] denotes a key sequence of length one.

    [(describe-function 'kbd)]
    [(Info-goto-node "(elisp)Key Sequences")]

    Here are some example inputs:

    "C-c"
    "C-M-q"
    "<f5>"
    "C-<f5>"
    "C-<right>"
    "<mouse-2>"
    "C-<down-mouse-3>" *)
val create_exn : string -> t

(** [(describe-function 'read-event)]
    [(Info-goto-node "(elisp)Reading One Event")] *)
val read : unit -> t

module Basic : sig
  type t =
    | Char_code of Char_code.t
    | Symbol    of Symbol.t
  [@@deriving sexp_of]
end

(** [(describe-function 'event-basic-type)]
    [(Info-goto-node "(elisp)Classifying Events")] *)
val basic : t -> Basic.t

module Modifier : sig
  type t =
    | Alt
    | Click
    | Control
    | Double
    | Down
    | Drag
    | Hyper
    | Meta
    | Shift
    | Super
    | Triple
  [@@deriving sexp_of]
end

(** [(describe-function 'event-modifiers)]
    [(Info-goto-node "(elisp)Classifying Events")] *)
val modifiers : t -> Modifier.t list

(** [(describe-variable 'unread-command-events)]
    [(Info-goto-node "(elisp)Event Input Misc")] *)
val unread_command_input : t list Var.t
val enqueue_unread_command_input : t list -> unit
