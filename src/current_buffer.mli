(** There are, in general, many buffers in an Emacs session.  At any time, one of them is
    designated as the "current buffer".  This is the buffer in which most editing takes
    place, because most of the primitives for examining or changing text in a buffer
    operate implicitly on the current buffer.  Normally the buffer that is displayed on
    the screen in the selected window is the current buffer, but this is not always so: a
    program can temporarily designate any buffer as current in order to operate on its
    contents, without changing what is displayed on the screen.

    [(Info-goto-node "(elisp)Current Buffer")]. *)

open! Core_kernel
open! Import

(** [(describe-function 'current-buffer)] *)
val get : unit -> Buffer.t

(** [(describe-function 'set-buffer)] *)
val set : Buffer.t -> unit

(** [(describe-variable 'default-directory)]
    [(Info-goto-node "(elisp)File Name Expansion")] *)
val directory : unit -> Filename.t
val set_directory : Filename.t -> unit

(** [(describe-variable 'buffer-undo-list)]
    [(Info-goto-node "(elisp)Undo")]
    [(Info-goto-node "(elisp)Maintaining Undo")] *)
val undo_list : unit -> Value.t
val is_undo_enabled : unit -> bool
val set_undo_enabled : bool -> unit

(** [(describe-function 'undo-boundary)]
    [(Info-goto-node "(elisp)Undo")] *)
val add_undo_boundary : unit -> unit

(** [(describe-function 'undo)] *)
val undo : int -> unit

(** [set_temporarily t ~f] runs [f] with the current buffer set to [t].
    [(describe-function 'with-current-buffer)]. *)
val set_temporarily : Buffer.t -> f:(unit -> 'a) -> 'a

(** [set_temporarily_to_temp_buffer f] creates a temporary buffer and runs [f] with the
    current buffer set to the temporary buffer.  [(describe-function 'with-temp-buffer)]. *)
val set_temporarily_to_temp_buffer : (unit -> 'a) -> 'a

(** [(describe-function 'buffer-modified-p)] *)
val is_modified : unit -> bool

(** [(describe-function 'set-buffer-modified-p)] *)
val set_modified : bool -> unit

(** [(describe-variable 'buffer-read-only)]
    [(Info-goto-node "(elisp)Read-Only Buffers")] *)
val is_read_only : unit -> bool
val set_read_only : bool -> unit

(** [(describe-function 'buffer-substring)]
    [(describe-function 'buffer-substring-no-properties)] *)
val contents
  :  ?start           : Position.t  (** default is [Point.min ()] *)
  -> ?end_            : Position.t  (** default is [Point.max ()] *)
  -> ?text_properties : bool        (** default is false *)
  -> unit
  -> Text.t

(** [(Info-goto-node "(elisp)Killing Buffers")]
    [(describe-function 'kill-buffer)] *)
val kill : unit -> unit

(** [(Info-goto-node "(elisp)Saving Buffers")]
    [(describe-function 'save-buffer)] *)
val save : unit -> unit

(** [(describe-function 'erase-buffer)] *)
val erase : unit -> unit

(** [(describe-function 'kill-region)] *)
val kill_region
  :  start : Position.t
  -> end_  : Position.t
  -> unit

(** [(describe-function 'save-excursion)] *)
val save_excursion : (unit -> 'a) -> 'a

(** [(describe-function 'set-buffer-multibyte)].
    [(Info-goto-node "(elisp)Selecting a Representation")]. *)
val set_multibyte : bool -> unit

(** [(describe-variable 'enable-multibyte-characters)]. *)
val is_multibyte : unit -> bool

(** [rename_exn] renames the current buffer, raising if [name] is already taken and
    [unique = false]; with [unique = true] it generates a new name.  [(describe-function
    'rename-buffer)]. *)
val rename_exn
  :  ?unique : bool
  -> unit
  -> name : string
  -> unit

(** [(describe-function 'put-text-property)]
    [(Info-goto-node "(elisp)Changing Properties")]. *)
val set_text_property
  :  ?start : Position.t  (** default is [Point.min ()] *)
  -> ?end_  : Position.t  (** default is [Point.max ()] *)
  -> 'a Text.Property_name.t
  -> 'a
  -> unit

(** [set_text_property_staged] is an optimization of [set_text_property] for the same text
    property on a number of regions, where the positions are [int]s.  It precomputes the
    Elisp property value once, and reuses it for each region. *)
val set_text_property_staged
  :  'a Text.Property_name.t
  -> 'a
  -> (start : int
      -> end_ : int
      -> unit) Staged.t

(** [(describe-function 'set-text-properties)]
    [(Info-goto-node "(elisp)Changing Properties")]. *)
val set_text_properties
  :  ?start : Position.t  (** default is [Point.min ()] *)
  -> ?end_  : Position.t  (** default is [Point.max ()] *)
  -> Text.Property.t list
  -> unit

(** [set_text_properties_staged] is an optimization of [set_text_properties] for the same
    text property on a number of regions, where the positions are [int]s.  It precomputes
    the Elisp property value once, and reuses it for each region. *)
val set_text_properties_staged
  :  Text.Property.t list
  -> (start : int
      -> end_ : int
      -> unit) Staged.t

(** [(describe-function 'add-text-properties)]
    [(Info-goto-node "(elisp)Changing Properties")]. *)
val add_text_properties
  :  ?start : Position.t  (** default is [Point.min ()] *)
  -> ?end_  : Position.t  (** default is [Point.max ()] *)
  -> Text.Property.t list
  -> unit

(** [add_text_properties_staged] is an optimization of [add_text_properties] for the same
    text property on a number of regions, where the positions are [int]s.  It precomputes
    the Elisp property value once, and reuses it for each region. *)
val add_text_properties_staged
  :  Text.Property.t list
  -> (start : int
      -> end_ : int
      -> unit) Staged.t

(** [text_property_is_present property_name] returns [true] if any text in the region from
    [start] to [end_] uses [property_name].  [(describe-function
    'text-property-not-all)] *)
val text_property_is_present
  :  ?start : Position.t  (** default is [Point.min ()] *)
  -> ?end_  : Position.t  (** default is [Point.max ()] *)
  -> _ Text.Property_name.t
  -> bool

(** [(describe-function 'set-marker)]
    [(Info-goto-node "(elisp)Moving Markers")] *)
val set_marker_position : Marker.t -> Position.t -> unit

(** [(describe-function 'mark-marker)]
    [(Info-goto-node "(elisp)The Mark")] *)
val get_mark : unit -> Marker.t

(** [(describe-function 'set-mark)]
    [(Info-goto-node "(elisp)The Mark")] *)
val set_mark : Position.t -> unit

(** [(describe-variable 'mark-active)]
    [(Info-goto-node "(elisp)The Mark")] *)
val mark_is_active : unit -> bool

(** [(describe-function 'deactivate-mark)]
    [(Info-goto-node "(elisp)The Mark")] *)
val deactivate_mark : unit -> unit

module Minor_mode : sig
  type t =
    { function_name : Symbol.t
    ; variable_name : Symbol.t }
  [@@deriving sexp_of]

  val is_enabled : t -> bool

  val disable : t -> unit
  val enable  : t -> unit

  (** [(describe-variable 'buffer-read-only)]
      [(describe-function 'read-only-mode)] *)
  val read_only : t

  (** [(describe-variable 'view-mode)]
      [(describe-function 'view-mode)] *)
  val view : t
end
