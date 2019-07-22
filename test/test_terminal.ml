open! Core_kernel
open! Async_kernel
open! Import
open! Terminal

let show t = print_s [%sexp (t : t)]
let shows ts = List.iter ts ~f:show
let current () = Frame.selected () |> Frame.terminal

let%expect_test "[all_live]" =
  shows (all_live ());
  [%expect {| "#<terminal 0 on initial_terminal>" |}];
  return ()
;;

let%expect_test "[Frame.terminal]" =
  show (current ());
  [%expect {| "#<terminal 0 on initial_terminal>" |}];
  return ()
;;

let%expect_test "[name]" =
  print_s [%sexp (name (current ()) : string)];
  [%expect {| initial_terminal |}];
  return ()
;;

let%expect_test "[graphical]" =
  print_s [%sexp (graphical (current ()) : bool)];
  [%expect {| false |}];
  return ()
;;
