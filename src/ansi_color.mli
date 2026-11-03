(** [Ansi_color] is a wrapper for [(find-library "ansi-color")]. *)

open! Core
open! Import

(** Calls [ansi-color-apply-on-region] with [ansi-color-context-region] bound to nil, so
    calls don't preserve state.

    Unlike plain [ansi-color-apply-on-region], which creates one overlay per colored span,
    this applies colors as [face] text properties (via
    [ecaml-ansi-color-apply-face-text-property]). Overlays are expensive in large buffers:
    each one is an Elisp heap object that the GC must trace, and large overlay counts slow
    down redisplay, whereas text properties are cheap and are discarded along with the
    text. The [face] text properties merge with faces applied by [add-face-text-property]
    the same way the overlays did. Don't use this in buffers that use font-lock
    fontification, which would overwrite the [face] properties. *)
val apply_on_region : start:Position.t -> end_:Position.t -> unit

val apply : string -> Text.t
val color_current_buffer : unit -> unit
