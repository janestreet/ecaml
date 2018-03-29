open! Core_kernel
open! Import

(** Tabulated lists display table data, one line per record and one column per field.

    Note that we attempt to improve upon the elisp interface of tabulated-list-mode. *)

module Column : sig
  (** A column of data to be displayed on screen. *)
  type 'record t

  val create
    :  ?align_right : bool
    -> ?pad_right   : int
    -> ?sortable    : bool
    -> header       : string
    -> width        : int
    -> ('record -> string)
    -> 'record t

end

type ('record, 'id) t

val create
  :  Source_code_position.t
  -> 'record Column.t list
  -> docstring           : string
  -> id_type             : 'id Value.Type.t
  -> id_of_record        : ('record -> 'id)
  -> initialize          : (unit -> unit)
  -> mode_change_command : Symbol.t
  -> mode_line           : string
  -> ('record, 'id) t

val major_mode : _ t -> Major_mode.t

val draw
  :  ?sort_by:(string * [`Ascending | `Descending])
  -> ('record, 'id) t
  -> 'record list
  -> unit

(** [get_id_at_point_exn] returns [None] if there is no id at point, and raises if the id
    at point cannot be [of_value_exn]'ed. *)
val get_id_at_point_exn : ('record, 'id) t -> 'id option
