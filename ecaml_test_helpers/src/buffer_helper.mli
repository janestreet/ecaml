open! Core
open! Import

(** [with_buffer_and_point contents { col; row } ~f] runs [f] in a temp buffer with
    [contents] and point at [row] and [col]. *)
val with_buffer_and_point
  :  (_, 'a) Sync_or_async.t
  -> string
  -> Line_and_column.t
  -> f:(unit -> 'a)
  -> 'a

val utf8_full_block_U2588 : string
val utf8_upper_left_U259B : string
val utf8_lower_right_U259F : string
val show_buffer : block_out:(Position.t * string) list -> unit

(** Print buffer contents with the point shown as a solid block. *)
val show_point : unit -> unit

(** Print buffer contents with the start and the end of the active region replaced by
    delimiter characters (double angle brackets). *)
val show_active_region : unit -> unit

(** Print buffer contents with the [before-string] and [after-string] properties from any
    overlays rendered as though they were present in the buffer. Also, mark any text
    specified as [invisible] through overlays (but not through text properties). This does
    not reflect the actual contents of the buffer, but rather how the contents would
    appear to the user. *)
val show_with_overlay_text : unit -> unit

module Sample_input : sig
  (** Both have "next step" columns, but with different bounds. *)
  val table1 : string

  val table2 : string
end
