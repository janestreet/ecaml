open! Core_kernel
open! Import
open! Frame

let show t = print_s [%sexp (t : t)]

let shows ts = List.iter ts ~f:show

let%expect_test "[all_live]" =
  shows (all_live ());
  [%expect {|
    "#<frame F1 0xbd9200>" |}];
;;

let%expect_test "[all_visible]" =
  shows (all_visible ());
  [%expect {|
    "#<frame F1 0xbd9200>" |}];
;;

let%expect_test "[selected]" =
  show (selected ());
  [%expect {|
    "#<frame F1 0xbd9200>" |}];
;;

let%expect_test "[set_selected]" =
  set_selected (selected ());
;;

let%expect_test "accessors" =
  let t = selected () in
  print_s [%message
    ""
      ~num_cols:    (num_cols     t : int)
      ~num_rows:    (num_rows     t : int)
      ~pixel_height:(pixel_height t : int)
      ~pixel_width: (pixel_height t : int)];
  [%expect {|
    ((num_cols     10)
     (num_rows     9)
     (pixel_height 10)
     (pixel_width  10)) |}];
;;

let%expect_test "accessors" =
  let t = selected () in
  print_s [%message
    ""
      ~parameters:(parameters t : (Symbol.t * Value.t) list)];
  [%expect {|
    (parameters (
      (menu-bar-lines     1)
      (buried-buffer-list nil)
      (buffer-list (#<buffer *scratch*>))
      (unsplittable     nil)
      (minibuffer       "#<window 2 on  *Minibuf-0*>")
      (modeline         t)
      (width            10)
      (height           9)
      (name             F1)
      (font             tty)
      (background-color unspecified-bg)
      (foreground-color unspecified-fg))) |}];
;;
