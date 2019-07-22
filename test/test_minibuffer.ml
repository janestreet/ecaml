open! Core_kernel
open! Async_kernel
open! Import
open! Minibuffer

let%expect_test "[y_or_n]" =
  let test input =
    with_input input (fun () ->
      let%bind bool = y_or_n ~prompt:"" in
      print_s [%sexp (bool : bool)];
      return ())
  in
  let%bind () = test "y" in
  [%expect {| (y or n) true |}];
  let%bind () = test "n" in
  [%expect {| (y or n) false |}];
  let%bind () = show_raise_async (fun () -> test "z") in
  [%expect
    {|
    (y or n) Please answer y or n.  (y or n) (raised ("Error reading from stdin")) |}];
  return ()
;;

let%expect_test "[y_or_n_with_timeout]" =
  let test input =
    with_input input (fun () ->
      let%bind response =
        y_or_n_with_timeout ~prompt:"" ~timeout:(Time_ns.Span.microsecond, 13)
      in
      print_s [%sexp (response : int Y_or_n_with_timeout.t)];
      return ())
  in
  let%bind () = test "y" in
  [%expect {| (y or n) Y |}];
  let%bind () = test "n" in
  [%expect {| (y or n) N |}];
  let%bind () = show_raise_async (fun () -> test "z") in
  [%expect
    {|
    (y or n) Please answer y or n.  (y or n) (raised ("Error reading from stdin")) |}];
  return ()
;;

let%expect_test "[yes_or_no]" =
  let test input =
    with_input input (fun () ->
      let%bind response = yes_or_no ~prompt:"" in
      print_s [%sexp (response : bool)];
      return ())
  in
  let%bind () = test "yes\n" in
  [%expect {| (yes or no) true |}];
  let%bind () = test "no\n" in
  [%expect {| (yes or no) false |}];
  return ()
;;

let%expect_test "[read_from]" =
  let%bind () =
    with_input "foo" (fun () ->
      let%bind response = read_from ~prompt:"" ~history:Minibuffer.history () in
      print_s [%sexp (response : string)];
      return ())
  in
  [%expect {| foo |}];
  return ()
;;

let%expect_test "[exit_hook]" =
  print_s [%sexp (exit_hook : _ Hook.t)];
  [%expect
    {|
    ((symbol    minibuffer-exit-hook)
     (hook_type Normal)
     (value (()))) |}];
  return ()
;;

let%expect_test "[setup_hook]" =
  print_s [%sexp (setup_hook : _ Hook.t)];
  [%expect
    {|
    ((symbol    minibuffer-setup-hook)
     (hook_type Normal)
     (value ((
       rfn-eshadow-setup-minibuffer
       minibuffer-history-isearch-setup
       minibuffer-history-initialize)))) |}];
  return ()
;;

let%expect_test "[setup_hook] [exit_hook] don't run in batch mode" =
  (* These hooks do work in an interactive Emacs.  But in batch mode, they appear to be
     ignored. *)
  Hook.add
    setup_hook
    (Hook.Function.create
       ("test-setup" |> Symbol.intern)
       [%here]
       ~hook_type:Normal
       (Returns Value.Type.unit)
       (fun () -> print_s [%message "running setup hook"]));
  Hook.add
    exit_hook
    (Hook.Function.create
       ("test-exit" |> Symbol.intern)
       [%here]
       ~hook_type:Normal
       (Returns Value.Type.unit)
       (fun () -> print_s [%message "running exit hook"]));
  let%bind () =
    with_input "foo" (fun () ->
      let%bind response = read_from ~prompt:"" ~history:Minibuffer.history () in
      print_s [%sexp (response : string)];
      return ())
  in
  [%expect {| foo |}];
  return ()
;;
