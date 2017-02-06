open! Core
open! Async
open! Import

let%expect_test "finalization of [Value.t]" =
  Gc.add_finalizer_exn (Value.of_int 13)
    (fun _ -> print_s [%message "finalized"]);
  Gc.full_major ();
  let%bind () = return () in (* to run Async finalizer *)
  [%expect {|
    finalized |}];
;;

let _garbage_collect =
  let f = "garbage-collect" |> Symbol.intern in
  fun () -> Symbol.funcall_i f []
;;

let () =
  Core.Unix.RLimit.(set core_file_size { (get core_file_size) with cur = Limit 0L })
;;

(* let%expect_test "finalization of an Emacs function" =
 *   let r = ref 14 in
 *   let f _ = r := 14; Value.nil in
 *   Gc.add_finalizer_exn f
 *     (fun _ -> print_s [%message "finalized" (r : int ref)]);
 *   ignore (Function.create f : Function.t);
 *   Gc.full_major ();
 *   let%bind () = after (sec 0.001) in
 *   garbage_collect ();
 *   Gc.full_major ();
 *   [%expect {|
 *     |}];
 * ;; *)
