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

val show_buffer : block_out:Position.t list -> unit

(** Print buffer contents with the point shown as a solid block.  *)
val show_point : unit -> unit

module Region : sig
  type t =
    { start : Line_and_column.t
    ; end_ : Line_and_column.t
    }
  [@@deriving sexp_of]
end

(** [with_buffer_and_active_region contents region ~f] runs [f] in a temp buffer with
    [contents], mark at [region.start], and point at [region.end_]. *)
val with_buffer_and_active_region
  :  (_, 'a) Sync_or_async.t
  -> string
  -> Region.t
  -> f:(unit -> 'a)
  -> 'a

(** Print buffer contents with the start and the end of the active region replaced by
    solid blocks. *)
val show_active_region : unit -> unit

(** Print buffer contents with the [before-string] and [after-string] properties from any
    overlays rendered as though they were present in the buffer.  Also, mark any text
    specified as [invisible] through overlays (but not through text properties).  This
    does not reflect the actual contents of the buffer, but rather how the contents would
    appear to the user. *)
val show_with_overlay_text : unit -> unit

module Sample_input : sig
  (** Both have "next step" columns, but with different bounds. *)
  val table1 : string

  val table2 : string
end
