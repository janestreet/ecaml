open! Core_kernel
open! Import

module Q = struct
  include Q

  let simple = "simple" |> Symbol.intern
end

module Current_buffer = Current_buffer0

let get = Funcall.("point" <: nullary @-> return Position.t)
let goto_char = Funcall.("goto-char" <: Position.t @-> return nil)
let goto_first_non_blank = Funcall.("back-to-indentation" <: nullary @-> return nil)
let min () = Generated_bindings.point_min () |> Position.of_int_exn
let max () = Generated_bindings.point_max () |> Position.of_int_exn
let goto_max () = goto_char (max ())
let goto_min () = goto_char (min ())
let beginning_of_line = Funcall.("beginning-of-line" <: nullary @-> return nil)
let end_of_line = Funcall.("end-of-line" <: nullary @-> return nil)
let forward_line = Funcall.("forward-line" <: int @-> return nil)

let forward_line_exn =
  let forward_line = Funcall.("forward-line" <: int @-> return int) in
  fun n ->
    match forward_line n with
    | 0 -> ()
    | _ -> raise_s [%sexp "Reached end of buffer"]
;;

let backward_line n = forward_line (-n)
let count_lines = Funcall.("count-lines" <: Position.t @-> Position.t @-> return int)
let count_lines ~start ~end_ = count_lines start end_

let goto_line l =
  goto_min ();
  forward_line (l - 1)
;;

let forward_char_exn = Funcall.("forward-char" <: int @-> return nil)
let backward_char_exn n = forward_char_exn (-n)
let delete_forward_char_exn = Funcall.("delete-char" <: int @-> return unit)
let delete_backward_char_exn n = delete_forward_char_exn (-n)
let forward_sexp_exn = Funcall.("forward-sexp" <: int @-> return nil)
let backward_sexp_exn = Funcall.("backward-sexp" <: int @-> return nil)
let forward_word n = ignore (Generated_bindings.forward_word n : bool)
let backward_word n = ignore (Generated_bindings.backward_word n : bool)
let following_char = Funcall.("following-char" <: nullary @-> return Char_code.t)

let line_number =
  Ecaml_value.Feature.require Q.simple;
  Funcall.("line-number-at-pos" <: nullary @-> return int)
;;

let is_beginning_of_buffer = Funcall.("bobp" <: nullary @-> return bool)
let is_end_of_buffer = Funcall.("eobp" <: nullary @-> return bool)
let column_number = Funcall.("current-column" <: nullary @-> return int)
let goto_column = Funcall.("move-to-column" <: int @-> return nil)

let get_line_and_column () =
  { Line_and_column.line = line_number (); column = column_number () }
;;

let goto_line_and_column { Line_and_column.line; column } =
  goto_line line;
  goto_column column
;;

let indent_line_to = Funcall.("indent-line-to" <: int @-> return nil)
let indent_line_to ~column = indent_line_to column
let insert = Funcall.("insert" <: string @-> return nil)
let insert_text = Funcall.("insert" <: Text.t @-> return nil)

let insert_file_contents =
  Funcall.(
    "insert-file-contents" <: string @-> nil @-> nil @-> nil @-> bool @-> return nil)
;;

let insert_file_contents_exn ?(replace = false) path =
  insert_file_contents path () () () replace
;;

let insert_file_contents_literally =
  Funcall.(
    "insert-file-contents-literally"
    <: string @-> nil @-> nil @-> nil @-> bool @-> return nil)
;;

let insert_file_contents_literally ?(replace = false) path =
  insert_file_contents_literally path () () () replace
;;

let kill_word = Funcall.("kill-word" <: int @-> return nil)
let marker_at = Funcall.("point-marker" <: nullary @-> return Marker.t)
let marker_at_min = Funcall.("point-min-marker" <: nullary @-> return Marker.t)
let marker_at_max = Funcall.("point-max-marker" <: nullary @-> return Marker.t)
let next_line = Funcall.("next-line" <: nullary @-> return nil)
let previous_line = Funcall.("previous-line" <: nullary @-> return nil)
let scroll_up = Funcall.("scroll-up" <: int @-> return nil)
let update_last_match_default = false

let handle_last_match ?(update_last_match = update_last_match_default) f =
  if not update_last_match
  then Regexp.Last_match.save f
  else (
    let result = f () in
    Regexp.Last_match.Private.Location.last
    := if result then Buffer (Current_buffer.get ()) else No_match;
    result)
;;

let search f ?bound ?update_last_match string =
  handle_last_match ?update_last_match (fun () -> f string bound true)
;;

let search_backward =
  search
    Funcall.("search-backward" <: string @-> nil_or Position.t @-> bool @-> return bool)
;;

let search_forward =
  search
    Funcall.("search-forward" <: string @-> nil_or Position.t @-> bool @-> return bool)
;;

let search_exn search ?bound ?update_last_match string =
  if not (search ?bound ?update_last_match string)
  then raise_s [%message "string not found" (string : string)]
;;

let search_backward_exn = search_exn search_backward
let search_forward_exn = search_exn search_forward

let search_regexp f ?bound ?update_last_match regexp =
  handle_last_match ?update_last_match (fun () -> f regexp bound true)
;;

let search_backward_regexp =
  search_regexp
    Funcall.(
      "search-backward-regexp" <: Regexp.t @-> nil_or Position.t @-> bool @-> return bool)
;;

let search_forward_regexp =
  search_regexp
    Funcall.(
      "search-forward-regexp" <: Regexp.t @-> nil_or Position.t @-> bool @-> return bool)
;;

let search_regexp_exn search_regexp ?bound ?update_last_match regexp =
  if not (search_regexp ?bound ?update_last_match regexp)
  then raise_s [%message "regexp not found" (regexp : Regexp.t)]
;;

let search_backward_regexp_exn = search_regexp_exn search_backward_regexp
let search_forward_regexp_exn = search_regexp_exn search_forward_regexp
let looking_at = Funcall.("looking-at" <: Regexp.t @-> return bool)
let looking_at_p = Funcall.("looking-at-p" <: Regexp.t @-> return bool)

let looking_at ?(update_last_match = update_last_match_default) regexp =
  handle_last_match ~update_last_match (fun () ->
    if update_last_match then looking_at regexp else looking_at_p regexp)
;;

let case_fold_search = Buffer_local.Wrap.("case-fold-search" <: bool)
let recenter = Funcall.("recenter" <: nil_or int @-> return nil)
let recenter ?screen_line () = recenter screen_line

let function_called_at =
  Funcall.("function-called-at-point" <: nullary @-> return (nil_or Symbol.t))
;;

let variable_at =
  (* [variable-at-point] returns 0 if there is no variable found. *)
  let variable_at_point = Funcall.("variable-at-point" <: nullary @-> return value) in
  fun () ->
    let ret = variable_at_point () in
    if Value.is_symbol ret then Some (Symbol.of_value_exn ret) else None
;;

let yank = Funcall.("yank" <: nullary @-> return nil)
