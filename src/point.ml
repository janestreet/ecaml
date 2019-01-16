open! Core_kernel
open! Import

module Q = struct
  include Q

  let back_to_indentation = "back-to-indentation" |> Symbol.intern
  and backward_sexp = "backward-sexp" |> Symbol.intern
  and beginning_of_line = "beginning-of-line" |> Symbol.intern
  and bobp = "bobp" |> Symbol.intern
  and count_lines = "count-lines" |> Symbol.intern
  and current_column = "current-column" |> Symbol.intern
  and delete_char = "delete-char" |> Symbol.intern
  and end_of_line = "end-of-line" |> Symbol.intern
  and eobp = "eobp" |> Symbol.intern
  and following_char = "following-char" |> Symbol.intern
  and forward_char = "forward-char" |> Symbol.intern
  and forward_line = "forward-line" |> Symbol.intern
  and forward_sexp = "forward-sexp" |> Symbol.intern
  and goto_char = "goto-char" |> Symbol.intern
  and indent_line_to = "indent-line-to" |> Symbol.intern
  and insert = "insert" |> Symbol.intern
  and insert_file_contents = "insert-file-contents" |> Symbol.intern
  and insert_file_contents_literally = "insert-file-contents-literally" |> Symbol.intern
  and kill_word = "kill-word" |> Symbol.intern
  and line_number_at_pos = "line-number-at-pos" |> Symbol.intern
  and looking_at = "looking-at" |> Symbol.intern
  and looking_at_p = "looking-at-p" |> Symbol.intern
  and move_to_column = "move-to-column" |> Symbol.intern
  and next_line = "next-line" |> Symbol.intern
  and previous_line = "previous-line" |> Symbol.intern
  and point_marker = "point-marker" |> Symbol.intern
  and point_max_marker = "point-max-marker" |> Symbol.intern
  and point_min_marker = "point-min-marker" |> Symbol.intern
  and recenter = "recenter" |> Symbol.intern
  and search_backward = "search-backward" |> Symbol.intern
  and search_backward_regexp = "search-backward-regexp" |> Symbol.intern
  and search_forward = "search-forward" |> Symbol.intern
  and search_forward_regexp = "search-forward-regexp" |> Symbol.intern
  and scroll_up = "scroll-up" |> Symbol.intern
  and simple = "simple" |> Symbol.intern
end

module F = struct
  open! Funcall
  open! Value.Type

  let back_to_indentation = Q.back_to_indentation <: nullary @-> return nil
  let count_lines = Q.count_lines <: Position.type_ @-> Position.type_ @-> return int
  let delete_char = Q.delete_char <: int @-> return unit
  let following_char = Q.following_char <: nullary @-> return Char_code.type_
  let forward_line = Q.forward_line <: int @-> return int
  let bobp = Q.bobp <: nullary @-> return bool
  let eobp = Q.eobp <: nullary @-> return bool

  let insert_file_contents_literally =
    Q.insert_file_contents_literally <: string @-> return nil
  ;;

  let next_line = Q.next_line <: nullary @-> return nil
  let previous_line = Q.previous_line <: nullary @-> return nil
  let recenter = Q.recenter <: option int @-> return nil
  let scroll_up = Q.scroll_up <: int @-> return nil
end

module Current_buffer = Current_buffer0

let get () = Symbol.funcall0 Q.point |> Position.of_value_exn
let goto_char t = Symbol.funcall1_i Q.goto_char (Position.to_value t)
let goto_first_non_blank () = F.back_to_indentation ()
let min () = Generated_bindings.point_min () |> Position.of_int_exn
let max () = Generated_bindings.point_max () |> Position.of_int_exn
let goto_max () = goto_char (max ())
let goto_min () = goto_char (min ())
let beginning_of_line () = Symbol.funcall0_i Q.beginning_of_line
let end_of_line () = Symbol.funcall0_i Q.end_of_line
let forward_line n = ignore (F.forward_line n : int)

let forward_line_exn n =
  match F.forward_line n with
  | 0 -> ()
  | _ -> raise_s [%sexp "Reached end of buffer"]
;;

let backward_line n = forward_line (-n)
let count_lines ~start ~end_ = F.count_lines start end_

let goto_line l =
  goto_min ();
  forward_line (l - 1)
;;

let forward_char_exn n = Symbol.funcall1_i Q.forward_char (n |> Value.of_int_exn)
let backward_char_exn n = forward_char_exn (-n)
let delete_forward_char_exn = F.delete_char
let delete_backward_char_exn n = delete_forward_char_exn (-n)
let forward_sexp_exn n = Symbol.funcall1_i Q.forward_sexp (n |> Value.of_int_exn)
let backward_sexp_exn n = Symbol.funcall1_i Q.backward_sexp (n |> Value.of_int_exn)
let forward_word n = ignore (Generated_bindings.forward_word n : bool)
let backward_word n = ignore (Generated_bindings.backward_word n : bool)
let following_char () = F.following_char ()

let line_number =
  Ecaml_value.Feature.require Q.simple;
  fun () -> Symbol.funcall0 Q.line_number_at_pos |> Value.to_int_exn
;;

let is_beginning_of_buffer = F.bobp
let is_end_of_buffer = F.eobp
let column_number () = Symbol.funcall0 Q.current_column |> Value.to_int_exn
let goto_column n = Symbol.funcall1_i Q.move_to_column (n |> Value.of_int_exn)

let indent_line_to ~column =
  Symbol.funcall1_i Q.indent_line_to (column |> Value.of_int_exn)
;;

let insert string = Symbol.funcall1_i Q.insert (string |> Value.of_utf8_bytes)
let insert_text text = Symbol.funcall1_i Q.insert (text |> Text.to_value)

let insert_file_contents_exn path =
  Symbol.funcall1_i Q.insert_file_contents (path |> Value.of_utf8_bytes)
;;

let insert_file_contents_literally path = F.insert_file_contents_literally path
let kill_word count = Symbol.funcall1_i Q.kill_word (count |> Value.of_int_exn)
let marker_at () = Symbol.funcall0 Q.point_marker |> Marker.of_value_exn
let marker_at_min () = Symbol.funcall0 Q.point_min_marker |> Marker.of_value_exn
let marker_at_max () = Symbol.funcall0 Q.point_max_marker |> Marker.of_value_exn
let next_line = F.next_line
let previous_line = F.previous_line
let scroll_up lines = F.scroll_up lines

let bound_value = function
  | None -> Value.nil
  | Some p -> p |> Position.to_value
;;

type 'a with_search_options = ?bound:Position.t -> ?update_last_match:bool -> 'a

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

let search q ?bound ?update_last_match string =
  handle_last_match ?update_last_match (fun () ->
    Symbol.funcall3 q (string |> Value.of_utf8_bytes) (bound |> bound_value) Value.t
    |> Value.to_bool)
;;

let search_backward = search Q.search_backward
let search_forward = search Q.search_forward

let search_exn q ?bound ?update_last_match string =
  if not (search q string ?bound ?update_last_match)
  then raise_s [%message "string not found" (string : string)]
;;

let search_backward_exn = search_exn Q.search_backward
let search_forward_exn = search_exn Q.search_forward

let search_regexp q ?bound ?update_last_match regexp =
  handle_last_match ?update_last_match (fun () ->
    Symbol.funcall3 q (regexp |> Regexp.to_value) (bound |> bound_value) Value.t
    |> Value.to_bool)
;;

let search_backward_regexp = search_regexp Q.search_backward_regexp
let search_forward_regexp = search_regexp Q.search_forward_regexp

let search_regexp_exn q ?bound ?update_last_match regexp =
  if not (search_regexp q regexp ?bound ?update_last_match)
  then raise_s [%message "regexp not found" (regexp : Regexp.t)]
;;

let search_backward_regexp_exn = search_regexp_exn Q.search_backward_regexp
let search_forward_regexp_exn = search_regexp_exn Q.search_forward_regexp

let looking_at ?(update_last_match = update_last_match_default) regexp =
  handle_last_match ~update_last_match (fun () ->
    Symbol.funcall1
      (if update_last_match then Q.looking_at else Q.looking_at_p)
      (regexp |> Regexp.to_value)
    |> Value.to_bool)
;;

let recenter ?screen_line () = F.recenter screen_line
