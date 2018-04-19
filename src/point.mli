(** "Point" is a special buffer position used by many editing commands.  Like other
    positions, point designates a place between two characters (or before the first
    character, or after the last character), rather than a particular character.  The
    value of point is a number no less than 1, and no greater than the buffer size plus 1.

    Each buffer has its own value of point, which is independent of the value of point in
    other buffers.  Each window also has a value of point, which is independent of the
    value of point in other windows on the same buffer.  This is why point can have
    different values in various windows that display the same buffer.  When a buffer
    appears in only one window, the buffer's point and the window's point normally have
    the same value, so the distinction is rarely important.

    [(Info-goto-node "(elisp)Point")]. *)

open! Core_kernel
open! Import

(** [(describe-function 'point)]. *)
val get : unit -> Position.t

(** [(describe-function 'point-min)]. *)
val min : unit -> Position.t

(** [(describe-function 'point-max)]. *)
val max : unit -> Position.t

(** [(describe-function 'goto-char)]. *)
val goto_char : Position.t -> unit

(** [goto_max () = goto_char (max ())] *)
val goto_max : unit -> unit

(** [goto_min () = goto_char (min ())] *)
val goto_min : unit -> unit

(** [(describe-function 'beginning-of-line)]. *)
val beginning_of_line : unit -> unit

(** [(describe-function 'end-of-line)]. *)
val end_of_line       : unit -> unit

(** [forward_char_exn n] moves point [n] characters forward (backward if [n] is negative).
    [forward_char_exn] raises on reaching end or beginning of buffer.  [(describe-function
    'forward-char)]. *)
val forward_char_exn : int -> unit

(** [backward_char_exn n = forward_char_exn (- n)]. *)
val backward_char_exn : int -> unit

(** [forward_line n] moves [n] lines forward (backward if [n] is negative).  Precisely, if
    point is on line [i], move to the start of line [i + n] ("start of line" in the
    logical order).  If there isn’t room, go as far as possible (no error).
    [(describe-function 'forward-line)].
    [(Info-goto-node "(elisp)Text Lines")] *)
val forward_line : int -> unit

(** [backward_line n = forward_line (- n)]. *)
val backward_line : int -> unit

(** [(describe-function 'forward-sexp)]
    [(Info-goto-node "(elisp)List Motion")] *)
val forward_sexp_exn : int -> unit

(** [(describe-function 'backward-sexp)]
    [(Info-goto-node "(elisp)List Motion")] *)
val backward_sexp_exn : int -> unit

(** [(describe-function 'forward-word)]
    [(Info-goto-node "(elisp)Word Motion")] *)
val forward_word : int -> unit

(** [(describe-function 'backward-word)]
    [(Info-goto-node "(elisp)Word Motion")] *)
val backward_word : int -> unit

(** [line_number] returns the line number of the character after point, where the first
    line of the buffer is line [1].
    [(describe-function 'line-number-at-pos)].
    [(Info-goto-node "(elisp)Text Lines")] *)
val line_number : unit -> int

(** [column_number] returns the colum of point, where the beginning of line is column 0.
    [(describe-function 'current-column)].
    [(Info-goto-node "(elisp)Columns")] *)
val column_number : unit -> int

(** [goto_column c] moves point to column [c] on the current line.
    [(describe-function 'move-to-column)].
    [(Info-goto-node "(elisp)Columns")] *)
val goto_column : int -> unit

(** [goto_line l = goto_min (); forward_line (l - 1)] *)
val goto_line : int -> unit

(** [(describe-function 'indent-line-to)] *)
val indent_line_to : column : int -> unit

(** [(describe-function 'insert)] *)
val insert      : string -> unit
val insert_text : Text.t -> unit

(** [(describe-function 'insert-file-contents)] *)
val insert_file_contents_exn : string -> unit

(** [(describe-function 'kill-word)]. *)
val kill_word : int -> unit

(** [(Info-goto-node "(elisp)Creating Markers")] *)
val marker_at     : unit -> Marker.t  (** [(describe-function 'point-marker)]. *)
val marker_at_min : unit -> Marker.t  (** [(describe-function 'point-min-marker)]. *)
val marker_at_max : unit -> Marker.t  (** [(describe-function 'point-max-marker)]. *)

(** There are eight search functions, varying by whether they:

    - search for a string or a regexp
    - search forward or backward from point
    - indicate success/failure by returning a boolean or raising on failure

    Searching backward sets point to the start of the match.  Searching forward sets point
    to the end of the match.  With [~bound], when searching backward, the match must start
    before [bound]; when searching forward, the match must end before [bound].  With
    [~update_last_match:true], searching updates [Regexp.Last_match].

    [(Info-goto-node "(elisp)String Search")]
    [(describe-function 'search-backward)]
    [(describe-function 'search-forward)]

    [(Info-goto-node "(elisp)Regexp Search")]
    [(describe-function 'search-backward-regexp)]
    [(describe-function 'search-forward-regexp)] *)

type 'a with_search_options
  =  ?bound             : Position.t  (** default is no bound *)
  -> ?update_last_match : bool        (** default is [false] *)
  -> 'a

val search_backward            : (string   -> bool) with_search_options
val search_backward_exn        : (string   -> unit) with_search_options
val search_forward             : (string   -> bool) with_search_options
val search_forward_exn         : (string   -> unit) with_search_options
val search_backward_regexp     : (Regexp.t -> bool) with_search_options
val search_backward_regexp_exn : (Regexp.t -> unit) with_search_options
val search_forward_regexp      : (Regexp.t -> bool) with_search_options
val search_forward_regexp_exn  : (Regexp.t -> unit) with_search_options

(** [looking_at regexp] returns [true] if the text after point matches [regexp].
    [(describe-function 'looking-at)]
    [(describe-function 'looking-at-p)]
    [(Info-goto-node "(elisp)Regexp Search")] *)
val looking_at
  :  ?update_last_match : bool  (** default is [false] *)
  -> Regexp.t
  -> bool
