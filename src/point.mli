(** "Point" is a special buffer position used by many editing commands. Like other
    positions, point designates a place between two characters (or before the first
    character, or after the last character), rather than a particular character. The value
    of point is a number no less than 1, and no greater than the buffer size plus 1.

    Each buffer has its own value of point, which is independent of the value of point in
    other buffers. Each window also has a value of point, which is independent of the
    value of point in other windows on the same buffer. This is why point can have
    different values in various windows that display the same buffer. When a buffer
    appears in only one window, the buffer's point and the window's point normally have
    the same value, so the distinction is rarely important.

    [(Info-goto-node "(elisp)Point")]. *)

open! Core
open! Import

(** [(describe-function 'point)]. *)
val get : unit -> Position.t

val get_line_and_column : unit -> Line_and_column.t

(** [(describe-function 'point-min)]. *)
val min : unit -> Position.t

(** [(describe-function 'point-max)]. *)
val max : unit -> Position.t

(** [(describe-function 'goto-char)]. *)
val goto_char : Position.t -> unit

(** Like [goto_char], but widen the buffer automatically if the target position is not in
    the accessible region of the buffer, and [widen-automatically] is non-nil.

    [(describe-variable 'widen-automatically)] *)
val goto_char_maybe_widen : Position.t -> unit

val goto_line_and_column : Line_and_column.t -> unit

(** [goto_max () = goto_char (max ())] *)
val goto_max : unit -> unit

(** [goto_min () = goto_char (min ())] *)
val goto_min : unit -> unit

(** [(describe-function 'beginning-of-line)]. *)
val beginning_of_line : unit -> unit

(** [(describe-function 'line-beginning-position)] *)
val beginning_of_line_position : unit -> Position.t

(** [(describe-function 'end-of-line)]. *)
val end_of_line : unit -> unit

(** [(describe-function 'line-end-position)] *)
val end_of_line_position : unit -> Position.t

(** [(describe-function 'count-lines)]. *)
val count_lines : start:Position.t -> end_:Position.t -> int

(** [(describe-function 'back-to-indentation)]. *)
val goto_first_non_blank : unit -> unit

(** [forward_char_exn n] moves point [n] characters forward (backward if [n] is negative).
    [forward_char_exn] raises on reaching end or beginning of buffer.

    [(describe-function 'forward-char)]. *)
val forward_char_exn : int -> unit

(** [backward_char_exn n = forward_char_exn (- n)]. *)
val backward_char_exn : int -> unit

(** [(describe-function 'delete-char)]. *)
val delete_forward_char_exn : int -> unit

(** [delete_backward_char_exn n = delete_forward_char_exn (- n)]. *)
val delete_backward_char_exn : int -> unit

(** [forward_line n] moves [n] lines forward (backward if [n] is negative). Precisely, if
    point is on line [i], move to the start of line [i + n] ("start of line" in the
    logical order). If there isn’t room, go as far as possible (no error).

    - [(describe-function 'forward-line)]
    - [(Info-goto-node "(elisp)Text Lines")] *)
val forward_line : int -> unit

(** [forward_line_exn n] is like [forward_line n], but it raises if it could not move the
    full [n] lines. *)
val forward_line_exn : int -> unit

(** [backward_line n = forward_line (- n)]. *)
val backward_line : int -> unit

(** - [(describe-function 'forward-sexp)]
    - [(Info-goto-node "(elisp)List Motion")] *)
val forward_sexp_exn : int -> unit

(** - [(describe-function 'backward-sexp)]
    - [(Info-goto-node "(elisp)List Motion")] *)
val backward_sexp_exn : int -> unit

(** - [(describe-function 'forward-word)]
    - [(Info-goto-node "(elisp)Word Motion")] *)
val forward_word : int -> unit

(** - [(describe-function 'backward-word)]
    - [(Info-goto-node "(elisp)Word Motion")] *)
val backward_word : int -> unit

(** [(describe-function 'following-char)] *)
val following_char : unit -> Char_code.t

(** [line_number] returns the line number of the character after point, where the first
    line of the buffer is line [1].

    This function always returns the absolute line number, regardless of any buffer
    narrowing. See the ABSOLUTE argument to [line-number-at-pos].

    - [(describe-function 'line-number-at-pos)]
    - [(Info-goto-node "(elisp)Text Lines")] *)
val line_number : unit -> int

(** [(describe-function 'bobp)] *)
val is_beginning_of_buffer : unit -> bool

(** [(describe-function 'eobp)] *)
val is_end_of_buffer : unit -> bool

(** [column_number] returns the colum of point, where the beginning of line is column 0.

    - [(describe-function 'current-column)]
    - [(Info-goto-node "(elisp)Columns")] *)
val column_number : unit -> int

(** [goto_column c] moves point to column [c] on the current line, where the beginning of
    the line is column 0.

    - [(describe-function 'move-to-column)]
    - [(Info-goto-node "(elisp)Columns")] *)
val goto_column : int -> unit

val position_of_line_and_column : Line_and_column.t -> Position.t

(** [goto_line l] moves point to the beginning of line number [l], where the first line in
    the buffer is numbered 1.

    [(describe-function 'goto-line)] *)
val goto_line : int -> unit

(** [(describe-function 'indent-line-to)] *)
val indent_line_to : column:int -> unit

(** [(describe-function 'insert)] *)
val insert : string -> unit

val insert_text : Text.t -> unit

(** [(describe-function 'insert-file-contents)] *)
val insert_file_contents_exn : ?replace:bool (** default is [false] *) -> string -> unit

(** [(describe-function 'insert-file-contents-literally)] *)
val insert_file_contents_literally
  :  ?replace:bool (** default is [false] *)
  -> string
  -> unit

(** [(describe-function 'kill-word)]. *)
val kill_word : int -> unit

(** [(Info-goto-node "(elisp)Creating Markers")] *)

(** [(describe-function 'point-marker)]. *)
val marker_at : unit -> Marker.t

(** [(describe-function 'point-min-marker)]. *)
val marker_at_min : unit -> Marker.t

(** [(describe-function 'point-max-marker)]. *)
val marker_at_max : unit -> Marker.t

(** There are eight search functions, varying by whether they:

    - search for a string or a regexp
    - search forward or backward from point
    - indicate success/failure by returning a boolean or raising on failure

    Searching backward sets point to the start of the match. Searching forward sets point
    to the end of the match. If the match fails, point is not moved. With [~bound], when
    searching backward, the match must start before [bound]; when searching forward, the
    match must end before [bound]. With [~update_last_match:true], searching updates
    [Regexp.Last_match].

    - [(Info-goto-node "(elisp)String Search")]
    - [(describe-function 'search-backward)]
    - [(describe-function 'search-forward)]

    - [(Info-goto-node "(elisp)Regexp Search")]
    - [(describe-function 'search-backward-regexp)]
    - [(describe-function 'search-forward-regexp)] *)

type 'a with_search_options :=
  ?bound:Position.t (** default is no bound *)
  -> ?update_last_match:bool (** default is [false] *)
  -> 'a

val search_backward : (string -> bool) with_search_options
val search_backward_exn : (string -> unit) with_search_options
val search_forward : (string -> bool) with_search_options
val search_forward_exn : (string -> unit) with_search_options
val search_backward_regexp : (Regexp.t -> bool) with_search_options
val search_backward_regexp_exn : (Regexp.t -> unit) with_search_options
val search_forward_regexp : (Regexp.t -> bool) with_search_options
val search_forward_regexp_exn : (Regexp.t -> unit) with_search_options

(** [(describe-variable 'case-fold-search)] *)
val case_fold_search : bool Buffer_local.t

(** [looking_at regexp] returns [true] if the text after point matches [regexp].

    - [(describe-function 'looking-at)]
    - [(describe-function 'looking-at-p)]
    - [(Info-goto-node "(elisp)Regexp Search")] *)
val looking_at : ?update_last_match:bool (** default is [false] *) -> Regexp.t -> bool

(** [(describe-function 'recenter)] *)
val recenter : ?screen_line:int -> unit -> unit

(** [(describe-function 'function-called-at-point)] *)
val function_called_at : unit -> Symbol.t option

(** [(describe-function 'variable-at-point)] *)
val variable_at : unit -> Symbol.t option

(** [(describe-function 'yank)] *)
val yank : unit -> unit

(** Search for text properties in the current buffer.

    [(find-library 'text-property-search)] *)
module Property_search : sig
  module Match : sig
    type 'a t = private
      { beginning : Position.t
      ; end_ : Position.t
      ; property_value : 'a
      }
    [@@deriving sexp_of]
  end

  module Which : sig
    type 'a t =
      | First_equal_to of 'a
      | First_non_nil
    [@@deriving enumerate, sexp_of]
  end

  (** [(describe-function 'text-property-search-forward)]

      If a match is found, moves point to the end of the match. *)
  val forward : 'a Text.Property_name.t -> which:'a Which.t -> 'a Match.t option
end
