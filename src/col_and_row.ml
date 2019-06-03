open! Core_kernel
open! Import

type t =
  { col : int
  ; row : int
  }
[@@deriving sexp_of]

let goto { col; row } =
  Point.goto_line row;
  Point.goto_column col
;;

let to_position t =
  Current_buffer.save_excursion Sync (fun () ->
    goto t;
    Point.get ())
;;

let of_position position =
  Current_buffer.save_excursion Sync (fun () ->
    Point.goto_char position;
    { col = Point.column_number (); row = Point.line_number () })
;;
