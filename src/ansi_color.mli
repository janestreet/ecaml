(** [Ansi_color] translates ANSI escape sequences to Emacs faces, like [(find-library
    "ansi-color")].  [Ansi_color] is significantly faster than [ansi-color], especially on
    larger inputs. *)

open! Core_kernel
open! Import

(** [color_current_buffer] modifies the current buffer, deleting ANSI color escapes and
    applying their corresponding Emacs faces to the regions they affect.  Saving the
    buffer after calling [color_current_buffer] would save without the escapes, which is
    probably not desired, so [color_current_buffer] marks the buffer as not modified.  It
    also turns off undo.  [color_current_buffer] is intended to replace for
    [Jane.ansi-color-buffer] and [Jane.ansi-color-region]. *)
val color_current_buffer
  :  ?start : Position.t
  -> ?end_  : Position.t
  -> unit
  -> unit

val color_text : Text.t -> Text.t

val max_supported_code : int

(** [(describe-variable 'ansi-color-names-vector].  [Colors] uses the colors in
    [ansi-colors-names-vector] when translating ANSI color codes to faces. *)
module Colors : sig
  type t [@@deriving sexp_of]

  val get : unit -> t
end

(** [print_state_machine = true] causes [color*] functions to [print_s] the
    state machine used for coloring.  This is used for testing. *)
val print_state_machine : bool ref

(** A customization variable governing whether invalid ANSI escape sequences are flagged
    in output. *)
val show_invalid_escapes : bool Var.t
