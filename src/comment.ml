open! Core_kernel
open! Import

let normalize_vars =
  (* Note: this function must be called in a buffer before reading from any of the vars
     below, or before calling any [comment-*] function.
     See [(describe-function 'comment-normalize-vars)]. *)
  Funcall.Wrap.("comment-normalize-vars" <: nullary @-> return nil)
;;

module Vars = struct
  let start = Var.Wrap.("comment-start" <: string)
  let start_regexp = Var.Wrap.("comment-start-skip" <: Regexp.t)
  let end_ = Var.Wrap.("comment-end" <: string)
  let end_regexp = Var.Wrap.("comment-end-skip" <: Regexp.t)
  let multi_line = Var.Wrap.("comment-multi-line" <: bool)
end

let wrap_var var () =
  normalize_vars ();
  Current_buffer.value_exn var
;;

let start = wrap_var Vars.start
let start_regexp = wrap_var Vars.start_regexp
let end_ = wrap_var Vars.end_
let end_regexp = wrap_var Vars.end_regexp
let multi_line = wrap_var Vars.multi_line

module Terminated_by = struct
  type t =
    | End_of_line (* line comments *)
    | Comment_end (* block comments *)
  [@@deriving sexp_of]

  let in_current_buffer () =
    match end_ () with
    | "" -> End_of_line
    | _ -> Comment_end
  ;;
end

let set_current_buffer_options ~start:s ~end_:e ~is_multi_line:m =
  Current_buffer.set_value Vars.start s;
  Current_buffer.set_value Vars.end_ e;
  Current_buffer.set_value Vars.multi_line m
;;

let beginning =
  let comment_beginning =
    Funcall.Wrap.("comment-beginning" <: nullary @-> return (nil_or Position.t))
  in
  fun () ->
    normalize_vars ();
    comment_beginning ()
;;

let goto_end_exn =
  let comment_enter_backward =
    Funcall.Wrap.("comment-enter-backward" <: nullary @-> return nil)
  in
  fun () ->
    normalize_vars ();
    let comment_end =
      match Terminated_by.in_current_buffer () with
      | Comment_end ->
        Current_buffer.save_excursion Sync (fun () ->
          match
            Point.search_forward_regexp (Current_buffer.value_exn Vars.end_regexp)
          with
          | false -> raise_s [%sexp "Could not find end of comment"]
          | true -> Point.get ())
      | End_of_line ->
        let rec loop ~prev_line_end =
          Point.forward_line 1;
          Point.end_of_line ();
          let eol = Point.get () in
          Point.beginning_of_line ();
          match Point.search_forward_regexp ~bound:eol (start_regexp ()) with
          | false -> prev_line_end
          | true -> loop ~prev_line_end:eol
        in
        Point.end_of_line ();
        loop ~prev_line_end:(Point.get ())
    in
    Point.goto_char comment_end;
    comment_enter_backward ()
;;
