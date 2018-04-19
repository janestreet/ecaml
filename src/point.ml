open! Core_kernel
open! Import

module Q = struct
  include Q
  let backward_sexp          = "backward-sexp"          |> Symbol.intern
  let beginning_of_line      = "beginning-of-line"      |> Symbol.intern
  let current_column         = "current-column"         |> Symbol.intern
  let end_of_line            = "end-of-line"            |> Symbol.intern
  let forward_char           = "forward-char"           |> Symbol.intern
  let forward_line           = "forward-line"           |> Symbol.intern
  let forward_sexp           = "forward-sexp"           |> Symbol.intern
  let goto_char              = "goto-char"              |> Symbol.intern
  let indent_line_to         = "indent-line-to"         |> Symbol.intern
  let insert                 = "insert"                 |> Symbol.intern
  let insert_file_contents   = "insert-file-contents"   |> Symbol.intern
  let kill_word              = "kill-word"              |> Symbol.intern
  let line_number_at_pos     = "line-number-at-pos"     |> Symbol.intern
  let looking_at             = "looking-at"             |> Symbol.intern
  let looking_at_p           = "looking-at-p"           |> Symbol.intern
  let move_to_column         = "move-to-column"         |> Symbol.intern
  let point_marker           = "point-marker"           |> Symbol.intern
  let point_max_marker       = "point-max-marker"       |> Symbol.intern
  let point_min_marker       = "point-min-marker"       |> Symbol.intern
  let search_backward        = "search-backward"        |> Symbol.intern
  let search_backward_regexp = "search-backward-regexp" |> Symbol.intern
  let search_forward         = "search-forward"         |> Symbol.intern
  let search_forward_regexp  = "search-forward-regexp"  |> Symbol.intern
  let simple                 = "simple"                 |> Symbol.intern
end

module Current_buffer = Current_buffer0

let get () = Symbol.funcall0 Q.point |> Position.of_value_exn

let goto_char t = Symbol.funcall1_i Q.goto_char (Position.to_value t)

let min () = Generated_bindings.point_min () |> Position.of_int_exn
let max () = Generated_bindings.point_max () |> Position.of_int_exn

let goto_max () = goto_char (max ())
let goto_min () = goto_char (min ())

let beginning_of_line () = Symbol.funcall0_i Q.beginning_of_line
let end_of_line       () = Symbol.funcall0_i Q.end_of_line

let forward_line n = Symbol.funcall1_i Q.forward_line (n |> Value.of_int_exn)

let backward_line n = forward_line (- n)

let goto_line l = goto_min (); forward_line (l - 1)

let forward_char_exn n = Symbol.funcall1_i Q.forward_char  (n |> Value.of_int_exn)

let backward_char_exn n = forward_char_exn (- n)

let forward_sexp_exn  n = Symbol.funcall1_i Q.forward_sexp  (n |> Value.of_int_exn)
let backward_sexp_exn n = Symbol.funcall1_i Q.backward_sexp (n |> Value.of_int_exn)

let forward_word  n = ignore (Generated_bindings.forward_word  n : bool)
let backward_word n = ignore (Generated_bindings.backward_word n : bool)

let line_number =
  Feature0.require Q.simple;
  fun () ->
    Symbol.funcall0 Q.line_number_at_pos |> Value.to_int_exn
;;

let column_number () = Symbol.funcall0 Q.current_column |> Value.to_int_exn

let goto_column n = Symbol.funcall1_i Q.move_to_column (n |> Value.of_int_exn)

let indent_line_to ~column =
  Symbol.funcall1_i Q.indent_line_to (column |> Value.of_int_exn)
;;

let insert      string = Symbol.funcall1_i Q.insert (string |> Value.of_utf8_bytes)
let insert_text text   = Symbol.funcall1_i Q.insert (text   |> Text.to_value)

let insert_file_contents_exn path =
  Symbol.funcall1_i Q.insert_file_contents (path |> Value.of_utf8_bytes)
;;

let kill_word count = Symbol.funcall1_i Q.kill_word (count |> Value.of_int_exn)

let marker_at     () = Symbol.funcall0 Q.point_marker     |> Marker.of_value_exn
let marker_at_min () = Symbol.funcall0 Q.point_min_marker |> Marker.of_value_exn
let marker_at_max () = Symbol.funcall0 Q.point_max_marker |> Marker.of_value_exn

let bound_value = function
  | None -> Value.nil
  | Some p -> p |> Position.to_value
;;

type 'a with_search_options
  =  ?bound             : Position.t
  -> ?update_last_match : bool
  -> 'a

let update_last_match_default = false

let handle_last_match ?(update_last_match = update_last_match_default) f =
  if not update_last_match
  then Regexp.Last_match.save f
  else (
    let result = f () in
    Regexp.Last_match.Private.Location.last :=
      if result then Buffer (Current_buffer.get ()) else No_match;
    result)
;;

let search q =
  fun ?bound ?update_last_match string ->
    handle_last_match ?update_last_match (fun () ->
      Symbol.funcall3 q
        (string |> Value.of_utf8_bytes)
        (bound |> bound_value)
        Value.t
      |> Value.to_bool)
;;

let search_backward = search Q.search_backward
let search_forward  = search Q.search_forward

let search_exn q =
  fun ?bound ?update_last_match string ->
    if not (search q string ?bound ?update_last_match)
    then raise_s [%message "string not found" (string : string)];
;;

let search_backward_exn = search_exn Q.search_backward
let search_forward_exn  = search_exn Q.search_forward

let search_regexp q =
  fun ?bound ?update_last_match regexp ->
    handle_last_match ?update_last_match (fun () ->
      Symbol.funcall3 q
        (regexp |> Regexp.to_value)
        (bound |> bound_value)
        Value.t
      |> Value.to_bool)
;;

let search_backward_regexp = search_regexp Q.search_backward_regexp
let search_forward_regexp  = search_regexp Q.search_forward_regexp

let search_regexp_exn q =
  fun ?bound ?update_last_match regexp ->
    if not (search_regexp q regexp ?bound ?update_last_match)
    then raise_s [%message "regexp not found" (regexp : Regexp.t)];
;;

let search_backward_regexp_exn = search_regexp_exn Q.search_backward_regexp
let search_forward_regexp_exn  = search_regexp_exn Q.search_forward_regexp

let looking_at ?(update_last_match = update_last_match_default) regexp =
  handle_last_match ~update_last_match (fun () ->
    Symbol.funcall1
      (if update_last_match
       then Q.looking_at
       else Q.looking_at_p)
      (regexp |> Regexp.to_value)
    |> Value.to_bool)
;;
