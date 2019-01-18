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

let quit =
  Or_error.try_with (fun () ->
    Clock.run_after (sec 0.01) Ecaml.Command.request_quit ();
    Async_ecaml.Private.block_on_async [%here] (fun () -> Deferred.never ()))
;;

let%expect_test "[block_on_async] with a quit" =
  print_s [%sexp (quit : _ Or_error.t)];
  [%expect {|
    (Error "Blocking operation interrupted") |}]
;;

let%expect_test "Nested calls to block_on_async raise" =
  show_raise ~hide_positions:true (fun () ->
    Async_ecaml.Private.block_on_async [%here] (fun () -> Deferred.unit));
  [%expect
    {|
    (raised (
      "Called [block_on_async] in the middle of an Async job!"
      (context_backtrace (
        (app/emacs/lib/ecaml/test/test_async.ml:LINE:COL ())
        (app/emacs/lib/ecaml/src/async_ecaml.ml:LINE:COL Expect_test_config.run))))) |}]
;;

let%expect_test "[defun Returns_unit_deferred] where body returns" =
  let symbol = "defun-return-unit-deferred" |> Symbol.intern in
  Defun.defun
    symbol
    [%here]
    Returns_unit_deferred
    (let open Defun.Let_syntax in
     let%map_open () = return () in
     message_s [%message "ran"];
     Deferred.unit);
  let%bind () =
    Async_ecaml.Private.run_outside_async (fun () ->
      Value.funcall0_i (symbol |> Symbol.to_value))
  in
  [%expect {|
    ran |}]
;;

let%expect_test "[defun Returns_unit_deferred] where body raises" =
  let symbol = "defun-return-unit-deferred" |> Symbol.intern in
  Defun.defun
    symbol
    [%here]
    Returns_unit_deferred
    (let open Defun.Let_syntax in
     let%map_open () = return () in
     raise_s [%message "ran"]);
  let%bind () =
    Async_ecaml.Private.run_outside_async (fun () ->
      try Value.funcall0_i (symbol |> Symbol.to_value) with
      | exn -> message_s [%message "caught" (exn : exn)])
  in
  [%expect {|
    (caught (exn (ran))) |}]
;;

let%expect_test "Nested calls to block_on_async raise, even via elisp" =
  let symbol = "block-on-async" |> Symbol.intern in
  Defun.defun
    symbol
    [%here]
    Returns_unit_deferred
    (let open Defun.Let_syntax in
     let%map_open () = return () in
     Deferred.unit);
  show_raise ~hide_positions:true (fun () -> Value.funcall0_i (symbol |> Symbol.to_value));
  [%expect
    {|
    (raised ((
      "Called [block_on_async] in the middle of an Async job!"
      (context_backtrace (
        (app/emacs/lib/ecaml/test/test_async.ml:LINE:COL (block-on-async))
        (app/emacs/lib/ecaml/src/async_ecaml.ml:LINE:COL Expect_test_config.run)))))) |}]
;;

(* We can't write a test of [block_on_async] succeeding because every test is already
   nested inside a [block_on_async] call through [Async_ecaml.Expect_test_config].
   However, that means every test implicitly tests [block_on_async]. *)

let%expect_test "raising to a try-with that has already returned" =
  let raise_error = Ivar.create () in
  match%bind
    try_with (fun () ->
      upon (Ivar.read raise_error) (fun () -> raise_s [%message "raising"]);
      return ())
  with
  | Error _ -> assert false
  | Ok () ->
    Ivar.fill raise_error ();
    let%bind () = Scheduler.yield_until_no_jobs_remain () in
    [%expect
      {|
    ("Exception raised to [Monitor.try_with] that already returned."
     (monitor.ml.Error raising ("<backtrace elided in test>"))) |}]
;;
