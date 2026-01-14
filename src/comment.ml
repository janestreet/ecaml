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

let find_start_of_single_line_comment_at_point () =
  Point.end_of_line ();
  Option.is_some (beginning ())
;;

let am_in_single_line_comment () =
  Current_buffer.save_excursion Sync (fun () ->
    find_start_of_single_line_comment_at_point ())
;;

let bounds_of_comment_at_point () =
  (* Try to find the bounds of the comment at point by searching both backwards and
     forwards for the ends of the comment. *)
  normalize_vars ();
  match Terminated_by.in_current_buffer () with
  | Comment_end ->
    let search dir for_ =
      let point = Point.get () in
      Current_buffer.save_excursion Sync (fun () ->
        Option.try_with (fun () ->
          let check, search =
            (* If the point is in the middle of a comment beginning/ending sequence,
               search_for/backward_regexp_exn won't find it. For example (if | represents
               the point):

               {v
                 /|* some comment */
               v}

               To get around this, we jump to the beginning/end of line and search for the
               first match that ends on the opposite side of where the point originally
               was. *)
            match dir with
            | `Forward ->
              Point.beginning_of_line ();
              ( (fun () -> Position.( <= ) (Point.get ()) point)
              , fun () -> Point.search_forward_regexp_exn for_ )
            | `Backward ->
              Point.end_of_line ();
              ( (fun () -> Position.( >= ) (Point.get ()) point)
              , fun () -> Point.search_backward_regexp_exn for_ )
          in
          while check () do
            search ()
          done;
          Point.get ()))
    in
    let start_regexp = Current_buffer.value_exn Vars.start_regexp in
    let end_regexp = Current_buffer.value_exn Vars.end_regexp in
    (match
       ( search `Backward end_regexp
       , search `Backward start_regexp
       , search `Forward end_regexp
       , search `Forward start_regexp )
     with
     | _, None, _, _ | _, _, None, _ ->
       (* In this case, we're not actually in a comment *)
       None
     | Some previous_end, Some comment_start, Some comment_end, Some next_start
       when Position.( >= ) previous_end comment_start
            || Position.( <= ) next_start comment_end ->
       (* We're between two comments. For example:
          {v /* comment 1 */ POINT /* comment 2 */ v} *)
       None
     | _, Some comment_start, Some comment_end, _ ->
       (* Otherwise, we must be in a comment, and [comment_start] and [comment_end] are
          the bounds of that comment. *)
       Some (comment_start, comment_end))
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
           ignore (find_start_of_single_line_comment_at_point () : bool);
           Point.get ())
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
