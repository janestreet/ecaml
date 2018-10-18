open! Core_kernel
open! Import
open! Async
open! Expect_test_helpers_kernel
open! Async_testing

let%expect_test "test basic async" =
  let%bind () = Clock.after (Time.Span.of_sec 0.1) in
  printf "Hello world\n";
  [%expect {| Hello world |}]
;;

let%expect_test "disk IO" =
  let temp_file = File.make_temp_file ~prefix:"test" ~suffix:"test" in
  let%bind () =
    Writer.with_file temp_file ~f:(fun writer ->
      Writer.write writer "Hello world";
      Deferred.unit)
  in
  let%bind () =
    Reader.with_file temp_file ~f:(fun reader ->
      let%map line = Reader.read_line reader in
      print_s [%sexp (line : string Reader.Read_result.t)])
  in
  [%expect {| (Ok "Hello world") |}]
;;

let timeout =
  Or_error.try_with (fun () ->
    Async_ecaml.Private.block_on_async
      ~timeout:(Some (sec 0.001))
      (fun () -> Deferred.never ()))
;;

let%expect_test "[block_on_async] with a timeout" =
  print_s [%sexp (timeout : _ Or_error.t)];
  [%expect {|
    (Error "Blocking operation timed out") |}]
;;

let quit =
  Or_error.try_with (fun () ->
    Clock.run_after (sec 0.01) Ecaml.Command.request_quit ();
    Async_ecaml.Private.block_on_async ~timeout:None (fun () -> Deferred.never ()))
;;

let%expect_test "[block_on_async] with a quit" =
  print_s [%sexp (quit : _ Or_error.t)];
  [%expect {|
    (Error "Blocking operation interrupted") |}]
;;

let%expect_test "Nested calls to block_on_async raise" =
  show_raise (fun () ->
    Async_ecaml.Private.block_on_async ~timeout:None (fun () -> Deferred.unit));
  [%expect {| (raised "Called [block_on_async] in the middle of an Async job!") |}]
;;

let%expect_test "Nested calls to block_on_async raise, even via elisp" =
  let symbol = "block-on-async" |> Symbol.intern in
  Defun.defun_blocking_async
    [%here]
    symbol
    (let open Defun.Let_syntax in
     let%map_open () = return () in
     Deferred.unit);
  show_raise (fun () -> Value.funcall0_i (symbol |> Symbol.to_value));
  [%expect {| (raised ("Called [block_on_async] in the middle of an Async job!")) |}]
;;

(* We can't write a test of [block_on_async] succeeding because every test is already
   nested inside a [block_on_async] call through [Async_ecaml.Expect_test_config].
   However, that means every test implicitly tests [block_on_async]. *)
