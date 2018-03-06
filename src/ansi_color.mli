(** [Ansi_color] translates ANSI escape sequences to Emacs faces, like [(find-library
    "ansi-color")].  [Ansi_color] is significantly faster than [ansi-color], especially on
    larger inputs. *)

open! Core_kernel
open! Import

(** [color_region_in_current_buffer] modifies the given region in the current buffer,
    deleting ANSI color escapes and applying their corresponding Emacs faces to the text
    they affect. [use_temp_file = true] can speed up coloring on large inputs but since it
    works by deleting and reinserting the region's text, it could drop existing text
    properties and markers in the region and maybe mess up the point. If [use_temp_file =
    false] then the point is preserved. *)
val color_region_in_current_buffer
  :  start : Position.t
  -> end_  : Position.t
  -> ?use_temp_file:bool (** default is false *)
  -> unit
  -> unit

(** [color_current_buffer] is similar to [color_region_in_current_buffer] where the region
    is the whole buffer but it always uses [use_temp_file = true]. Saving the buffer after
    calling [color_current_buffer] would save without the escapes, which is probably not
    desired, so [color_current_buffer] marks the buffer as not modified.  It also turns
    off undo. After colorization point is set to the beginning of the buffer. *)
val color_current_buffer : unit -> unit

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
