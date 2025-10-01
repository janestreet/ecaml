(** Tabulated lists display table data, one line per record and one column per field.

    Note that we attempt to improve upon the elisp interface of tabulated-list-mode. *)

open! Core
open! Import

val major_mode : Major_mode.t

module Column : sig
  (** A column of data to be displayed on screen. *)
  type 'record t

  (** Each time we [draw] the buffer, the column is resized to fit the widest value (or
      the header), within the range [min_width, max_width] inclusive. *)
  val create
    :  ?align_right:bool
    -> ?max_width:int
    -> ?min_width:int
    -> ?pad_right:int
    -> ?sortable:bool
    -> header:string
    -> ('record -> string)
    -> 'record t

  val text
    :  ?align_right:bool
    -> ?max_width:int
    -> ?min_width:int
    -> ?pad_right:int
    -> ?sortable:bool
    -> header:string
    -> ('record -> Text.t)
    -> 'record t

  val create_gen
    :  ?align_right:bool
    -> ?max_width:int
    -> ?min_width:int
    -> ?pad_right:int
    -> header:string
    -> get_field:('record -> 'field)
    -> render_field:('field -> Text.t)
    -> compare_field:('field -> 'field -> int)
    -> unit
    -> 'record t
end

type 'record t

(** [get_id] is used to extract an ID value from each row. This ID is used to, among other
    things, preserve the position of point on a given row when the list is redrawn. The ID
    string must not be empty, due to implementation constraints. *)
val create : 'record Column.t list -> get_id:('record -> string) -> 'record t

(** The major mode in the current buffer must be derived from tabulated-list-mode. *)
val draw
  :  ?sort_by:string * [ `Ascending | `Descending ]
  -> 'record t
  -> 'record list
  -> unit

(** [get_record_at_point_exn] returns [None] if there is no record at point, and raises if
    the record at point cannot be [of_value_exn]'ed. *)
val get_record_at_point_exn : 'record t -> 'record option

(** [move_point_to_record] moves the point to the first record stisfying the predicate [f] *)
val move_point_to_record : 'record t -> f:('record -> bool) -> unit

(** [(describe-variable 'tabulated-list-revert-hook)] *)
val revert_hook : (Hook.normal, unit) Hook.t
