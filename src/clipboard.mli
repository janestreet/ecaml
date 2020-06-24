(** For dealing with the GUI's clipboard. *)

open! Core_kernel
open! Async_kernel
open! Import

(** [kill_new text] makes [text] the latest kill in the kill ring, and sets the clipboard
    to contain [text]. *)
val kill_new : Text.t -> unit

(** [yank_at_point ()] inserts the clipboard's contents at point. *)
val yank_at_point : unit -> unit
