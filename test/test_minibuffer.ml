open! Core_kernel
open! Import
open! Minibuffer

let%expect_test "[y_or_n]" =
  let test input =
    with_input input (fun () -> print_s [%sexp (y_or_n ~prompt:"" : bool)]) in
  test "y";
  [%expect {|
    (y or n) true |}];
  test "n";
  [%expect {|
    (y or n) false |}];
  show_raise (fun () -> test "z");
  [%expect {|
    (y or n) Please answer y or n.  (y or n) (raised ("Error reading from stdin")) |}];
;;

let%expect_test "[y_or_n_with_timeout]" =
  let test input =
    with_input input (fun () ->
      print_s [%sexp
        (y_or_n_with_timeout ~prompt:"" ~timeout:(Time_ns.Span.microsecond, 13)
         : int Y_or_n_with_timeout.t)]) in
  test "y";
  [%expect {|
    (y or n) Y |}];
  test "n";
  [%expect {|
    (y or n) N |}];
  show_raise (fun () -> test "z");
  [%expect {|
    (y or n) Please answer y or n.  (y or n) (raised ("Error reading from stdin")) |}];
;;

let%expect_test "[yes_or_no]" =
  let test input =
    with_input input (fun () -> print_s [%sexp (yes_or_no ~prompt:"" : bool)]) in
  test "yes\n";
  [%expect {|
    (yes or no) true |}];
  test "no\n";
  [%expect {|
    (yes or no) false |}];
;;

let%expect_test "[read_from]" =
  with_input "foo" (fun () ->
    print_s [%sexp (read_from () ~prompt:"" : string)]);
  [%expect {|
    foo |}];
;;

let%expect_test "[exit_hook]" =
  print_s [%sexp (exit_hook : _ Hook.t)];
  [%expect {|
    ((symbol minibuffer-exit-hook)
     (type_  Normal)
     (value ())) |}];
;;

let%expect_test "[setup_hook]" =
  print_s [%sexp (setup_hook : _ Hook.t)];
  [%expect {|
    ((symbol minibuffer-setup-hook)
     (type_  Normal)
     (value (
       rfn-eshadow-setup-minibuffer
       minibuffer-history-isearch-setup
       minibuffer-history-initialize))) |}];
;;

let%expect_test "[setup_hook] [exit_hook] don't run in batch mode" =
  (* These hooks do work in an interactive Emacs.  But in batch mode, they appear to be
     ignored. *)
  Hook.add setup_hook
    (Hook.Function.create [%here] Normal ("test-setup" |> Symbol.intern)
       (fun () -> print_s [%message "running setup hook"]));
  Hook.add exit_hook
    (Hook.Function.create [%here] Normal ("test-exit" |> Symbol.intern)
       (fun () -> print_s [%message "running exit hook"]));
  with_input "foo" (fun () ->
    print_s [%sexp (read_from () ~prompt:"" : string)]);
  [%expect {|
    foo |}];
;;
