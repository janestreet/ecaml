(** Tabulated lists display table data, one line per record and one column per field.

    Note that we attempt to improve upon the elisp interface of tabulated-list-mode. *)

open! Core_kernel
open! Import
module Tabulated_list_mode : Major_mode.S

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

  (** Shows only stripped first line of possibly multiline string *)
  val first_line
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
end

type ('record, 'id) t

(** Raises unless the major mode derives from [Tabulated_list_mode.major_mode]. *)
val create
  :  Major_mode.t
  -> 'record Column.t list
  -> id_equal:('id -> 'id -> bool)
  -> id_type:'id Value.Type.t
  -> id_of_record:('record -> 'id)
  -> ('record, 'id) t

val keymap : _ t -> Keymap.t
val major_mode : _ t -> Major_mode.t

val draw
  :  ?sort_by:string * [ `Ascending | `Descending ]
  -> ('record, 'id) t
  -> 'record list
  -> unit

(** [get_id_at_point_exn] returns [None] if there is no id at point, and raises if the id
    at point cannot be [of_value_exn]'ed. *)
val get_id_at_point_exn : ('record, 'id) t -> 'id option

val move_point_to_id : ('record, 'id) t -> 'id -> unit
val current_buffer_has_entries : unit -> bool

(** [(describe-variable 'tabulated-list-revert-hook)] *)
val revert_hook : Hook.normal Hook.t
