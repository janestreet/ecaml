(** The "echo area" is used for displaying error messages for messages made with the
    `message' primitive, and for echoing keystrokes.  It is not the same as the
    minibuffer, despite the fact that the minibuffer appears (when active) in the same
    place on the screen as the echo area.

    [(Info-goto-node "(elisp)The Echo Area")]. *)

open! Core_kernel
open! Import

val message       : string -> unit
val messagef      : ('a, unit, string, unit) format4 -> 'a
val message_s     : Sexp.t -> unit
val message_value : Value.t -> unit

(** [(describe-variable 'inhibit-message)] *)
val inhibit_messages : (unit -> 'a) -> 'a
