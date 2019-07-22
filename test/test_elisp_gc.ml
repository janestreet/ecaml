open! Core_kernel
open! Async_kernel
open! Import
open! Elisp_gc

let%expect_test "[gcs_done], [garbage_collect]" =
  let before = gcs_done () in
  garbage_collect ();
  let after = gcs_done () in
  print_s [%sexp (after - before : int)];
  [%expect {| 1 |}];
  return ()
;;

let%expect_test "[gc_elapsed]" =
  let span1 = gc_elapsed () in
  garbage_collect ();
  let span2 = gc_elapsed () in
  print_s [%sexp (Time_ns.Span.( < ) span1 span2 : bool)];
  [%expect {| true |}];
  return ()
;;

let%expect_test "[post_gc_hook]" =
  Hook.add
    post_gc_hook
    (Hook.Function.create
       ("test-hook" |> Symbol.intern)
       [%here]
       ~hook_type:Normal
       (Returns Value.Type.unit)
       (fun () -> print_s [%message "ran"]));
  garbage_collect ();
  [%expect {| ran |}];
  return ()
;;
