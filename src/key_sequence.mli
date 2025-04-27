(** A key sequence is a sequence of one or more input events that form a unit. Input
    events include characters, function keys, and mouse actions.

    [(Info-goto-node "(elisp)Key Sequences")] *)

open! Core
open! Async_kernel
open! Import
include Value.Subtype with type t = Key_sequence0.t

val length : t -> int
val get : t -> int -> Input_event.t

(** [(describe-function 'listify-key-sequence)]
    [(Info-goto-node "(elisp)Event Input Misc")] *)
val to_list : t -> Input_event.t list

val of_list : Input_event.t list -> t

(** - [(describe-function 'read-kbd-macro)]
    - [(Info-goto-node "(elisp)Key Sequences")]
    - [(Info-goto-node "(elisp)Decribing Characters")]

    Here are some example inputs:

    - ["C-c y"]
    - ["C-M-q"]
    - ["<f5>"]
    - ["C-<f5>"]
    - ["C-<right>"]
    - ["<mouse-2>"]
    - ["C-<down-mouse-3>"] *)
val create_exn : string -> t

(** - [(describe-function 'execute-kbd-macro)]
    - [(Info-goto-node "(elisp)Keyboard Macros")]

    {b Warning}: [execute-kbd-macro] sets the current buffer to the selected window's
    buffer. *)
val execute : t -> unit Deferred.t

(** - [(describe-variable 'executing-kbd-macro)]
    - [(Info-goto-node "(elisp)Keyboard Macros")] *)
val am_executing : bool Var.t

(** - [(describe-function 'read-key-sequence-vector)]
    - [(Info-goto-node "(elisp)Key Sequence Input")] *)
val read : unit -> prompt:string -> t

(** - [(describe-variable 'unread-command-events)]
    - [(Info-goto-node "(elisp)Event Input Misc")] *)
val enqueue_unread_command_input : t -> unit

val sigusr1 : t

(** [(describe-function 'key-description)] *)
val description : t -> string

val concat : t list -> t

(** [(describe-function 'this-command-keys)] *)
val invoking_this_command : unit -> t
