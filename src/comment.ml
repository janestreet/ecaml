open! Core
open! Import

let normalize_vars =
  (* Note: this function must be called in a buffer before reading from any of the vars
     below, or before calling any [comment-*] function. See
     [(describe-function 'comment-normalize-vars)]. *)
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

let am_in_single_line_comment () =
  Current_buffer.save_excursion Sync (fun () ->
    Point.end_of_line ();
    Option.is_some (beginning ()))
;;

module Syntax = struct
  (* These functions all require that [comment-use-syntax] is non-nil, i.e., the syntax
     table for the major mode can be reliably used to distinguish comments. (That is not
     the case, for, e.g., org-mode). *)

  let forward_comment = Funcall.Wrap.("forward-comment" <: int @-> return bool)
  let syntax_ppss = Funcall.Wrap.("syntax-ppss" <: nullary @-> return value)

  let ppss_comment_or_string_start =
    Funcall.Wrap.("ppss-comment-or-string-start" <: value @-> return (nil_or Position.t))
  ;;

  let syntax_ppss_comment_start () = ppss_comment_or_string_start (syntax_ppss ())

  let bounds_of_comment_at_point () =
    (* Use [syntax-ppss] to find the comment start, and [forward-comment] from there to
       find the end. *)
    let%bind.Option comment_start = syntax_ppss_comment_start () in
    let%bind.Option comment_end =
      Current_buffer.save_excursion Sync (fun () ->
        Point.goto_char comment_start;
        if forward_comment 1 then Some (Point.get ()) else None)
    in
    Some (comment_start, comment_end)
  ;;
end

let bounds_of_comment_at_point () =
  (* Try to find the bounds of the comment at point by searching both backwards and
     forwards for the ends of the comment. *)
  normalize_vars ();
  match Terminated_by.in_current_buffer () with
  | Comment_end ->
    (* This [Comment_end] branch is not reached for known modes where [comment-use-syntax]
       is nil (e.g., org-mode), since those modes have [comment-end = ""] and use the
       [End_of_line] branch. Therefore, it's fine to rely on the syntax table-based logic. *)
    Syntax.bounds_of_comment_at_point ()
  | End_of_line ->
    (match am_in_single_line_comment () with
     | false -> None
     | true ->
       (* Unlike emacs, we consider a sequence of consecutive line comments as a single
          comment. *)
       let rec search dir ~last_point =
         Point.forward_line
           (match dir with
            | `Backward -> -1
            | `Forward -> 1);
         match am_in_single_line_comment () with
         | false -> last_point
         | true ->
           if Position.( <> ) last_point (Point.get ())
           then search dir ~last_point:(Point.get ())
           else Point.get ()
       in
       let comment_start =
         Current_buffer.save_excursion Sync (fun () ->
           Point.goto_char (search `Backward ~last_point:(Point.get ()));
           Point.end_of_line ();
           (* [beginning] moves point inside the comment (after the comment-start marker)
              but returns the position of the comment-starter itself. We use the returned
              position so that the start marker is included in the bounds. *)
           match beginning () with
           | Some pos -> pos
           | None -> Point.get ())
       in
       let comment_end =
         Current_buffer.save_excursion Sync (fun () ->
           Point.goto_char (search `Forward ~last_point:(Point.get ()));
           Point.end_of_line ();
           Point.get ())
       in
       Some (comment_start, comment_end))
;;

let goto_beginning_exn () =
  let comment_beginning =
    match bounds_of_comment_at_point () with
    | None -> raise_s [%sexp "not in a comment"]
    | Some (beginning, _) -> beginning
  in
  Point.goto_char comment_beginning;
  try
    let eol =
      Current_buffer.save_excursion Sync (fun () ->
        Point.end_of_line ();
        Point.get ())
    in
    let search_forward () =
      Point.search_forward_regexp_exn
        ~bound:eol
        (Current_buffer.value_exn Vars.start_regexp)
    in
    try search_forward () with
    | _ ->
      Point.beginning_of_line ();
      search_forward ()
  with
  | _ -> Point.goto_char comment_beginning
;;

let goto_end_exn =
  let comment_enter_backward =
    Funcall.Wrap.("comment-enter-backward" <: nullary @-> return nil)
  in
  fun () ->
    let comment_end =
      match bounds_of_comment_at_point () with
      | None -> raise_s [%sexp "not in a comment"]
      | Some (_, end_) -> end_
    in
    Point.goto_char comment_end;
    comment_enter_backward ()
;;
