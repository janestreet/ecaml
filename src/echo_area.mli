(** The "echo area" is used for displaying error messages for messages made with the
    `message' primitive, and for echoing keystrokes. It is not the same as the minibuffer,
    despite the fact that the minibuffer appears (when active) in the same place on the
    screen as the echo area.

    Messages are displayed in both the echo area and logged to the *Messages* buffer. Use
    [inhibit_messages] or [~echo:false] to cause them to only be logged in *Messages*.

    [(Info-goto-node "(elisp)The Echo Area")]. *)

open! Core
open! Async_kernel
open! Import0

val message : ?echo:bool (** default is [true] *) -> string -> unit

(** [clear ()] clears the echo area, like [(message nil)]. See
    [(describe-function 'message)] *)
val clear : unit -> unit

val wrap_message
  :  ?allow_in_background:bool (** default is [false] *)
  -> ?echo:bool (** default is [true] *)
  -> ?show_in_tests:bool (** default is [true] *)
  -> here:[%call_pos]
  -> (_, 'a) Sync_or_async.t
  -> string
  -> f:(unit -> 'a)
  -> 'a

val messagef
  :  ?echo:bool (** default is [true] *)
  -> ('a, unit, string, unit) format4
  -> 'a

val message_s : ?echo:bool (** default is [true] *) -> Sexp.t -> unit
val message_text : ?echo:bool (** default is [true] *) -> Text.t -> unit

(** [(describe-variable 'inhibit-message)] *)
val inhibit_messages : (_, 'a) Sync_or_async.t -> (unit -> 'a) -> 'a
