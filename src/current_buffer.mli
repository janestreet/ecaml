(** There are, in general, many buffers in an Emacs session. At any time, one of them is
    designated as the "current buffer". This is the buffer in which most editing takes
    place, because most of the primitives for examining or changing text in a buffer
    operate implicitly on the current buffer. Normally the buffer that is displayed on the
    screen in the selected window is the current buffer, but this is not always so: a
    program can temporarily designate any buffer as current in order to operate on its
    contents, without changing what is displayed on the screen.

    [(Info-goto-node "(elisp)Current Buffer")]. *)

open! Core
open! Async_kernel
open! Import
open! Ecaml_filename
include Current_buffer0_intf.Current_buffer0_public

val get_buffer_local : 'a Buffer_local.t -> 'a
val get_buffer_local_exn : 'a option Buffer_local.t -> 'a
val set_buffer_local : 'a Buffer_local.t -> 'a -> unit

val set_buffer_local_temporarily
  :  (_, 'a) Sync_or_async.t
  -> 'b Buffer_local.t
  -> 'b
  -> f:(unit -> 'a)
  -> 'a

(** - [(describe-variable 'default-directory)]
    - [(Info-goto-node "(elisp)File Name Expansion")] *)
val directory : Filename.t option Buffer_local.t

(** The same as [directory], but better-typed. *)
val directory_abspath : File_path.Absolute.t option Buffer_local.t

(** - [(describe-variable 'buffer-undo-list)]
    - [(Info-goto-node "(elisp)Undo")]
    - [(Info-goto-node "(elisp)Maintaining Undo")] *)
val undo_list : unit -> Value.t

val is_undo_enabled : unit -> bool
val set_undo_enabled : bool -> unit

(** - [(describe-function 'buffer-file-name)]
    - [(Info-goto-node "(elisp)Buffer File Name")] *)
val file_name : unit -> string option

val file_name_exn : unit -> string

(** [(describe-function 'buffer-name)] *)
val name : unit -> string

(** - [(describe-variable 'buffer-file-name)]
    - [(Info-goto-node "(elisp)Buffer File Name")] *)
val file_name_var : Filename.t option Buffer_local.t

(** The same as [file_name_var], but better-typed. *)
val file_name_abspath : File_path.Absolute.t option Buffer_local.t

module Coding_system : sig
  type t =
    | Utf_8
    | Utf_8_unix
end

(** [(describe-variable 'file-coding-system)] *)
val file_coding_system : Coding_system.t option Buffer_local.t

(** - [(describe-function 'undo-boundary)]
    - [(Info-goto-node "(elisp)Undo")] *)
val add_undo_boundary : unit -> unit

(** [(describe-function 'undo)] *)
val undo : int -> unit

(** [set_temporarily_to_temp_buffer f] creates a temporary buffer and runs [f] with the
    current buffer set to the temporary buffer. [(describe-function 'with-temp-buffer)]. *)
val set_temporarily_to_temp_buffer
  :  here:[%call_pos]
  -> ?name:string (** passed to [generate-new-buffer] *)
  -> (_, 'a) Sync_or_async.t
  -> (unit -> 'a)
  -> 'a

(** - [(describe-function 'bury-buffer)]
    - [(Info-goto-node "(elisp)Buffer List")] *)
val bury : unit -> unit

(** [(describe-function 'buffer-modified-p)] *)
val is_modified : unit -> bool

(** [(describe-function 'set-buffer-modified-p)] *)
val set_modified : bool -> unit

(** - [(describe-variable 'fill-column)]
    - [(Info-goto-node "(elisp)Margins")] *)
val fill_column : int Buffer_local.t

(** - [(describe-variable 'paragraph-start)]
    - [(Info-goto-node "(elisp)Standard Regexps ")] *)
val paragraph_start : Regexp.t Var.t

(** - [(describe-variable 'paragraph-separate)]
    - [(Info-goto-node "(elisp)Standard Regexps ")] *)
val paragraph_separate : Regexp.t Var.t

(** - [(describe-variable 'buffer-read-only)]
    - [(Info-goto-node "(elisp)Read-Only Buffers")] *)
val read_only : bool Buffer_local.t

(** [(describe-variable 'show-trailing-whitespace)] *)
val show_trailing_whitespace : bool Buffer_local.t

(** - [(describe-variable 'transient-mark-mode)]
    - [(describe-function 'transient-mark-mode)]
    - [(Info-goto-node "(elisp)The Mark")] *)
val transient_mark_mode : bool Customization.t

(** - [(describe-variable 'major-mode)]
    - [(Info-goto-node "(elisp)Auto Major Mode")] *)
val major_mode : unit -> Major_mode.t

val change_major_mode : Major_mode.t -> unit Deferred.t

(** [(describe-function 'set-auto-mode)] *)
val set_auto_mode : ?keep_mode_if_same:bool -> unit -> unit Deferred.t

(** - [(describe-function 'make-local-variable)]
    - [(Info-goto-node "(elisp)Creating Buffer-Local")] *)
val make_buffer_local : _ Var.t -> unit

(** - [(describe-function 'local-variable-p)]
    - [(Info-goto-node "(elisp)Creating Buffer-Local")] *)
val is_buffer_local : _ Var.t -> bool

(** - [(describe-function 'local-variable-if-set-p)]
    - [(Info-goto-node "(elisp)Creating Buffer-Local")] *)
val is_buffer_local_if_set : _ Var.t -> bool

(** - [(describe-function 'buffer-local-variables)]
    - [(Info-goto-node "(elisp)Creating Buffer-Local")] *)
val buffer_local_variables : unit -> (Symbol.t * Value.t option) list

(** - [(describe-function 'kill-local-variable)]
    - [(Info-goto-node "(elisp)Creating Buffer-Local")] *)
val kill_buffer_local : _ Var.t -> unit

(** - [(describe-function 'current-local-map)]
    - [(Info-goto-node "(elisp)Controlling Active Maps")] *)
val local_keymap : unit -> Keymap.t option

(** - [(describe-function 'use-local-map)]
    - [(Info-goto-node "(elisp)Controlling Active Maps")] *)
val set_local_keymap : Keymap.t -> unit

(** - [(describe-function 'current-minor-mode-maps)]
    - [(Info-goto-node "(elisp)Controlling Active Maps")] *)
val minor_mode_keymaps : unit -> Keymap.t list

(** - [(describe-function 'buffer-substring)]
    - [(describe-function 'buffer-substring-no-properties)] *)
val contents
  :  ?start:Position.t (** default is [Point.min ()] *)
  -> ?end_:Position.t (** default is [Point.max ()] *)
  -> ?text_properties:bool (** default is false *)
  -> unit
  -> Text.t

(** - [(Info-goto-node "(elisp)Killing Buffers")]
    - [(describe-function 'kill-buffer)] *)
val kill : unit -> unit Deferred.t

(** - [(Info-goto-node "(elisp)Saving Buffers")]
    - [(describe-function 'save-buffer)] *)
val save : unit -> unit Deferred.t

(** [(describe-function 'erase-buffer)] *)
val erase : unit -> unit

(** - [(describe-function 'delete-region)]
    - [(Info-goto-node "(elisp)Deletion")] *)
val delete_region : start:Position.t -> end_:Position.t -> unit

(** [(describe-function 'kill-region)] *)
val kill_region : start:Position.t -> end_:Position.t -> unit

(** - [(describe-function 'narrow-to-region)]
    - [(Info-goto-node "(elisp)Narrowing")] *)
val narrow_to_region : start:Position.t -> end_:Position.t -> unit

(** [(describe-function 'save-current-buffer)] *)
val save_current_buffer
  :  here:[%call_pos]
  -> (_, 'a) Sync_or_async.t
  -> (unit -> 'a)
  -> 'a

(** [(describe-function 'save-excursion)] *)
val save_excursion : here:[%call_pos] -> (_, 'a) Sync_or_async.t -> (unit -> 'a) -> 'a

(** [(describe-function 'save-mark-and-excursion)] *)
val save_mark_and_excursion
  :  here:[%call_pos]
  -> (_, 'a) Sync_or_async.t
  -> (unit -> 'a)
  -> 'a

(** [(describe-function 'save-restriction)] *)
val save_restriction : here:[%call_pos] -> (_, 'a) Sync_or_async.t -> (unit -> 'a) -> 'a

(** - [(describe-function 'set-buffer-multibyte)]
    - [(Info-goto-node "(elisp)Selecting a Representation")] *)
val set_multibyte : bool -> unit

(** [(describe-variable 'enable-multibyte-characters)]. *)
val is_multibyte : unit -> bool

(** - [(describe-function 'put-text-property)]
    - [(Info-goto-node "(elisp)Changing Properties")] *)
val set_text_property
  :  ?start:Position.t (** default is [Point.min ()] *)
  -> ?end_:Position.t (** default is [Point.max ()] *)
  -> 'a Text.Property_name.t
  -> 'a
  -> unit

(** [set_text_property_staged] is an optimization of [set_text_property] for the same text
    property on a number of regions, where the positions are [int]s. It precomputes the
    Elisp property value once, and reuses it for each region. *)
val set_text_property_staged
  :  'a Text.Property_name.t
  -> 'a
  -> (start:int -> end_:int -> unit) Staged.t

(** - [(describe-function 'set-text-properties)]
    - [(Info-goto-node "(elisp)Changing Properties")]. *)
val set_text_properties
  :  ?start:Position.t (** default is [Point.min ()] *)
  -> ?end_:Position.t (** default is [Point.max ()] *)
  -> Text.Property.t list
  -> unit

(** [set_text_properties_staged] is an optimization of [set_text_properties] for the same
    text property on a number of regions, where the positions are [int]s. It precomputes
    the Elisp property value once, and reuses it for each region. *)
val set_text_properties_staged
  :  Text.Property.t list
  -> (start:int -> end_:int -> unit) Staged.t

(** [(describe-function 'get-text-property)] *)
val get_text_property : Position.t -> 'a Text.Property_name.t -> 'a option

(** [(describe-function 'get-char-property)] *)
val get_char_property : Position.t -> 'a Text.Property_name.t -> 'a option

(** - [(describe-function 'add-text-properties)]
    - [(Info-goto-node "(elisp)Changing Properties")]. *)
val add_text_properties
  :  ?start:Position.t (** default is [Point.min ()] *)
  -> ?end_:Position.t (** default is [Point.max ()] *)
  -> Text.Property.t list
  -> unit

(** [add_text_properties_staged] is an optimization of [add_text_properties] for the same
    text property on a number of regions, where the positions are [int]s. It precomputes
    the Elisp property value once, and reuses it for each region. *)
val add_text_properties_staged
  :  Text.Property.t list
  -> (start:int -> end_:int -> unit) Staged.t

(** - [(describe-function 'add-face-text-property)]
    - [(Info-goto-node "(elisp)Changing Properties")]. *)
val add_face_text_properties
  :  ?start:Position.t (** default is [Point.min ()] *)
  -> ?end_:Position.t (** default is [Point.max ()] *)
  -> ?append:bool (** default is false *)
  -> Text.Face_spec.t
  -> unit

(** [text_property_is_present property_name] returns [true] if any text in the region from
    [start] to [end_] uses [property_name]. [(describe-function 'text-property-not-all)] *)
val text_property_is_present
  :  ?start:Position.t (** default is [Point.min ()] *)
  -> ?end_:Position.t (** default is [Point.max ()] *)
  -> _ Text.Property_name.t
  -> bool

(** - [(describe-function 'set-marker)]
    - [(Info-goto-node "(elisp)Moving Markers")] *)
val set_marker_position : Marker.t -> Position.t -> unit

(** - [(describe-function 'mark-marker)]
    - [(Info-goto-node "(elisp)The Mark")] *)
val mark : unit -> Marker.t

(** - [(describe-function 'set-mark)]
    - [(Info-goto-node "(elisp)The Mark")] *)
val set_mark : Position.t -> unit

(** - [(describe-variable 'mark-active)]
    - [(Info-goto-node "(elisp)The Mark")] *)
val mark_is_active : unit -> bool

(** - [(describe-function 'region-beginning)]
    - [(describe-function 'region-end)]
    - [(describe-function 'use-region-p)]

    Returns [(region-beginning), (region-end)] if the region should be used (according to
    [use-region-p]). *)
val active_region : unit -> (Position.t * Position.t) option

(** - [(describe-function 'deactivate-mark)]
    - [(Info-goto-node "(elisp)The Mark")] *)
val deactivate_mark : unit -> unit

(** [(describe-function 'flush-lines)] *)
val delete_lines_matching
  :  ?start:Position.t (** default is [Point.min ()] *)
  -> ?end_:Position.t (** default is [Point.max ()] *)
  -> Regexp.t
  -> unit

(** - [(describe-function 'sort-lines)]
    - [(Info-goto-node "(elisp)Sorting")] *)
val sort_lines
  :  ?start:Position.t (** default is [Point.min ()] *)
  -> ?end_:Position.t (** default is [Point.max ()] *)
  -> unit
  -> unit

(** [(describe-function 'delete-duplicate-lines)] *)
val delete_duplicate_lines
  :  ?start:Position.t (** default is [Point.min ()] *)
  -> ?end_:Position.t (** default is [Point.max ()] *)
  -> unit
  -> unit

(** - [(describe-function 'indent-region)]
    - [(Info-goto-node "(elisp)Region Indent")] *)
val indent_region
  :  ?start:Position.t (** default is [Point.min ()] *)
  -> ?end_:Position.t (** default is [Point.max ()] *)
  -> unit
  -> unit

(** - [(describe-function 'revert-buffer)]
    - [(Info-goto-node "(elisp)Reverting")] *)
val revert : ?confirm:bool (** default is [false] *) -> unit -> unit Deferred.t

(** [(describe-function 'replace-buffer-contents)] *)
val replace_buffer_contents
  :  ?max_duration:Time_ns.Span.t
  -> ?max_costs:int
  -> Buffer.t
  -> unit

(** [replace_string ~from ~to_] replaces all occurrences of [from] with [to_]. It is like
    [(describe-function 'replace-string)], but doesn't actually call that, because its
    documentation says:

    {v
      This function is for interactive use only;
      in Lisp code use `search-forward' and `replace-match' instead.
    v} *)
val replace_string
  :  ?start:Position.t (** default is [Point.min ()] *)
  -> ?end_:Position.t (** default is [Point.max ()] *)
  -> from:string
  -> to_:string
  -> unit
  -> unit

(** [(describe-function 'buffer-size)] *)
val size : unit -> int

(** [(describe-variable 'truncate-lines)] *)
val truncate_lines : bool Buffer_local.t

(** - [(describe-function 'buffer-chars-modified-tick)]
    - [(Info-goto-node "(elisp)Buffer Modification")] *)
val chars_modified_tick : unit -> Modified_tick.t

(** [append_to string] appends [string] to the end of the current buffer, and preserves
    point-at-max for windows that display it. *)
val append_to : string -> unit

val inhibit_read_only : (_, 'a) Sync_or_async.t -> (unit -> 'a) -> 'a
val line_and_column_of_position : Position.t -> Line_and_column.t
val position_of_line_and_column : Line_and_column.t -> Position.t

(** [(describe-function 'key-binding)] *)
val key_binding : Key_sequence.t -> Keymap.Entry.t

(** [(describe-function 'derived-mode-p)] *)
val is_derived_mode : Major_mode.t list -> bool

(** [(describe-function 'write-region)]

    This passes ['nomessage] for the VISIT argument. *)
val write_region
  :  ?start:Position.t (** default is [Point.min ()] *)
  -> ?end_:Position.t (** default is [Point.max ()] *)
  -> ?append:bool (** default is [false] *)
  -> Filename.t
  -> unit

(** [(describe-function 'buffer-disable-undo)] *)
val disable_undo : unit -> unit
