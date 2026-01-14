(** [Ansi_color] is a wrapper for [(find-library "ansi-color")]. *)

open! Core
open! Import

(** Calls [ansi-color-apply-on-region] with [ansi-color-context-region] bound to nil, so
    calls don't preserve state. *)
val apply_on_region : start:Position.t -> end_:Position.t -> unit

val apply : string -> Text.t
val color_current_buffer : unit -> unit
