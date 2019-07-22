open! Core_kernel
open! Async_kernel
open! Import
open! Timer

let test f =
  let continue = ref true in
  run_after_i [%here] (sec_ns 0.) ~name:(Symbol.gensym ()) ~f:(fun () ->
    print_s [%message "ran"];
    continue := false);
  while_ (fun () -> !continue) ~do_:(fun () -> f (sec_ns 0.001))
;;

let%expect_test "[run_after], [sit_for]" =
  let%bind () = test sit_for in
  [%expect {| ran |}];
  return ()
;;

let%expect_test "[run_after], [sleep_for]" =
  let%bind () = test sleep_for in
  [%expect {| ran |}];
  return ()
;;

let%expect_test "[run_after ~repeat]" =
  let r = ref 0 in
  let continue = ref true in
  run_after_i
    [%here]
    (sec_ns 0.)
    ~repeat:(sec_ns 0.001)
    ~name:(Symbol.gensym ())
    ~f:(fun () ->
      if !r = 3
      then continue := false
      else (
        incr r;
        print_s [%message "" ~_:(!r : int)]));
  let%bind () = while_ (fun () -> !continue) ~do_:(fun () -> sit_for (sec_ns 0.001)) in
  [%expect {|
    1
    2
    3 |}];
  return ()
;;

let%expect_test "[cancel], [is_scheduled]" =
  let t = run_after [%here] (sec_ns 60.) ~name:(Symbol.gensym ()) ~f:ignore in
  print_s [%sexp (is_scheduled t : bool)];
  [%expect {| true |}];
  cancel t;
  print_s [%sexp (is_scheduled t : bool)];
  [%expect {| false |}];
  return ()
;;
