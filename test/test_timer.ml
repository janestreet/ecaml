open! Core_kernel
open! Import
open! Timer

let sec = Time_ns.Span.of_sec

let test f =
  let continue = ref true in
  run_after_i (sec 0.) (fun () -> print_s [%message "ran"]; continue := false);
  while !continue do
    f (sec 0.001);
  done;
;;

let%expect_test "[run_after], [sit_for]" =
  test sit_for;
  [%expect {|
    ran |}]
;;

let%expect_test "[run_after], [sleep_for]" =
  test sleep_for;
  [%expect {|
    ran |}]
;;

let%expect_test "[run_after ~repeat]" =
  let r = ref 0 in
  let continue = ref true in
  run_after_i (sec 0.) ~repeat:(sec 0.001) (fun () ->
    if !r = 3
    then continue := false
    else (
      incr r;
      print_s [%message "" ~_:(!r : int)]));
  while !continue do
    sit_for (sec 0.001);
  done;
  [%expect {|
    1
    2
    3 |}];
;;

let%expect_test "[cancel], [is_scheduled]" =
  let t = run_after (sec 60.) ignore in
  print_s [%sexp (is_scheduled t : bool)];
  [%expect {|
    true |}];
  cancel t;
  print_s [%sexp (is_scheduled t : bool)];
  [%expect {|
    false |}];
;;
