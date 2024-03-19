open! Core
open! Async_kernel
open! Import
open! Frame

let redact_hex_number_in_string =
  let pattern =
    let open Re in
    compile (seq [ str "0x"; rep1 xdigit ])
  in
  Re.replace_string pattern ~by:"REDACTED"
;;

let redact_hex_number_in_sexp =
  smash_sexp ~f:(function
    | List _ as sexp -> sexp
    | Atom atom -> Atom (redact_hex_number_in_string atom))
;;

let show t = print_s (redact_hex_number_in_sexp [%sexp (t : t)])
let shows ts = List.iter ts ~f:show

let%expect_test "[all_live]" =
  shows (all_live ());
  [%expect {| "#<frame F1 REDACTED>" |}];
  return ()
;;

let%expect_test "[all_visible]" =
  shows (all_visible ());
  [%expect {| "#<frame F1 REDACTED>" |}];
  return ()
;;

let%expect_test "[selected]" =
  show (selected ());
  [%expect {| "#<frame F1 REDACTED>" |}];
  return ()
;;

let%expect_test "[set_selected]" =
  set_selected (selected ());
  return ()
;;

let%expect_test "accessors" =
  let t = selected () in
  print_s
    [%message
      ""
        ~num_cols:(num_cols t : int)
        ~num_rows:(num_rows t : int)
        ~pixel_height:(pixel_height t : int)
        ~pixel_width:(pixel_height t : int)];
  [%expect
    {|
    ((num_cols     80)
     (num_rows     25)
     (pixel_height 25)
     (pixel_width  25))
    |}];
  return ()
;;

let%expect_test "accessors" =
  let t = selected () in
  print_s [%message "" ~parameters:(parameters t : (Symbol.t * Value.t) list)];
  [%expect
    {|
    (parameters (
      (tab-bar-lines      0)
      (menu-bar-lines     1)
      (buried-buffer-list nil)
      (buffer-list ("#<buffer *scratch*>"))
      (unsplittable          nil)
      (modeline              t)
      (width                 80)
      (height                25)
      (name                  F1)
      (font                  tty)
      (background-color      unspecified-bg)
      (foreground-color      unspecified-fg)
      (cursor-color          white)
      (scroll-bar-background nil)
      (scroll-bar-foreground nil)
      (background-mode       dark)
      (display-type          mono)
      (minibuffer            t)))
    |}];
  return ()
;;
