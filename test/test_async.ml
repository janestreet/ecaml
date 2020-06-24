open! Core_kernel
open! Async
open! Import

let%expect_test "test basic async" =
  let%bind () = Clock.after (Time.Span.of_sec 0.1) in
  printf "Hello world\n";
  [%expect {| Hello world |}];
  return ()
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
  [%expect {| (Ok "Hello world") |}];
  return ()
;;

let%expect_test "[block_on_async] with a quit" =
  (* We want to test what happens when Emacs calls into Ecaml that does a
     [block_on_async], and a [quit] comes in while [block_on_async] is blocked.
     [let%expect] is already doing a [block_on_async], so we first need to
     [run_outside_async].  We then call an Ecaml [defun]'d function that
     [Returns_deferred], which, under the hood, does [block_on_async] that deferred. *)
  let fname = "async-ecaml-test-quit-in-block-on-async" in
  let () =
    defun_nullary
      (fname |> Symbol.intern)
      [%here]
      (Returns_deferred Value.Type.unit)
      (fun () ->
         Clock.run_after (sec 0.01) Ecaml.Command.request_quit ();
         Deferred.never ())
  in
  let%bind quit =
    Async_ecaml.Private.run_outside_async [%here] (fun () ->
      Or_error.try_with Funcall.Wrap.(fname <: nullary @-> return nil))
  in
  print_s [%sexp (quit : _ Or_error.t)];
  [%expect {|
    (Error ("Blocking operation interrupted")) |}];
  return ()
;;

let%expect_test "Nested calls to block_on_async raise" =
  show_raise ~hide_positions:true (fun () ->
    Async_ecaml.Private.block_on_async [%here] (fun () -> Deferred.unit));
  [%expect
    {|
    (raised (
      "Called [block_on_async] in the middle of an Async job!"
      (context_backtrace (
        ((here app/emacs/lib/ecaml/test/test_async.ml:LINE:COL)
         (context ())
         (created_at _))
        ((here app/emacs/lib/ecaml/src/async_ecaml.ml:LINE:COL)
         (context    Expect_test_config.run)
         (created_at _)))))) |}];
  return ()
;;

let%expect_test "Nested calls to block_on_async include Nested_profile backtraces" =
  show_raise ~hide_positions:true (fun () ->
    let clock = Nested_profile.Profile.Private.Clock.create ~now:Time_ns.epoch in
    Ref.set_temporarily Nested_profile.Profile.Private.clock clock ~f:(fun () ->
      Ref.set_temporarily Nested_profile.Profile.should_profile true ~f:(fun () ->
        Nested_profile.profile
          Sync
          (lazy [%message "Some badly-behaved function"])
          (fun () ->
             Async_ecaml.Private.block_on_async [%here] (fun () -> Deferred.unit)))));
  [%expect
    {|
    (raised (
      "Called [block_on_async] in the middle of an Async job!"
      (context_backtrace (
        ((here app/emacs/lib/ecaml/test/test_async.ml:LINE:COL)
         (context ())
         (created_at _))
        ((here app/emacs/lib/ecaml/src/async_ecaml.ml:LINE:COL)
         (context    Expect_test_config.run)
         (created_at _))))
      (profile_backtrace ("Some badly-behaved function")))) |}];
  return ()
;;

let%expect_test "[defun (Returns_deferred Value.Type.(deferred unit))] where body returns"
  =
  let symbol = "defun-return-unit-deferred" |> Symbol.intern in
  Defun.defun
    symbol
    [%here]
    (Returns_deferred Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open () = return () in
     message_s [%message "ran"];
     Deferred.unit);
  let%bind () =
    Async_ecaml.Private.run_outside_async [%here] (fun () ->
      Value.funcall0_i (symbol |> Symbol.to_value))
  in
  [%expect {|
    ran |}];
  return ()
;;

let%expect_test "[defun (Returns_deferred Value.Type.(deferred unit))] where body raises"
  =
  let symbol = "defun-return-unit-deferred" |> Symbol.intern in
  Defun.defun
    symbol
    [%here]
    (Returns_deferred Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open () = return () in
     raise_s [%message "ran"]);
  let%bind () =
    Async_ecaml.Private.run_outside_async [%here] (fun () ->
      try Value.funcall0_i (symbol |> Symbol.to_value) with
      | exn -> message_s [%message "caught" (exn : exn)])
  in
  [%expect {|
    (caught (exn (ran))) |}];
  return ()
;;

let%expect_test "defun returning non-unit deferred" =
  let symbol = "defun-return-deferred" |> Symbol.intern in
  Defun.defun
    symbol
    [%here]
    (Returns_deferred Value.Type.int)
    (let open Defun.Let_syntax in
     let%map_open () = return () in
     message_s [%message "ran"];
     Deferred.return 23);
  let%bind output =
    Async_ecaml.Private.run_outside_async [%here] (fun () ->
      Value.funcall0 (symbol |> Symbol.to_value) |> Value.Type.(of_value_exn int))
  in
  print_s [%sexp (output : int)];
  [%expect {|
    ran
    23 |}];
  return ()
;;

let%expect_test "Nested calls to block_on_async raise, even via elisp" =
  let symbol = "block-on-async" |> Symbol.intern in
  Defun.defun
    symbol
    [%here]
    (Returns_deferred Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open () = return () in
     Deferred.unit);
  show_raise ~hide_positions:true (fun () ->
    Value.funcall0_i (symbol |> Symbol.to_value));
  [%expect
    {|
    (raised ((
      "Called [block_on_async] in the middle of an Async job!"
      (context_backtrace (
        ((here app/emacs/lib/ecaml/test/test_async.ml:LINE:COL)
         (context (block-on-async))
         (created_at _))
        ((here app/emacs/lib/ecaml/src/async_ecaml.ml:LINE:COL)
         (context    Expect_test_config.run)
         (created_at _))))))) |}];
  return ()
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
       (monitor.ml.Error raising ("<backtrace elided in test>"))) |}];
    return ()
;;

let%expect_test "assert_foreground" =
  Background.assert_foreground [%here] ~message:[%message "should not raise"];
  [%expect {||}];
  let%bind () =
    Deferred.create (fun background_job_complete ->
      Background.don't_wait_for [%here] (fun () ->
        show_raise ~hide_positions:true (fun () ->
          Background.assert_foreground [%here] ~message:[%message "should raise"]);
        Ivar.fill background_job_complete ();
        return ()))
  in
  [%expect
    {|
    (raised (
      "Assertion failed -- running in background job"
      ((background_job_started_at app/emacs/lib/ecaml/test/test_async.ml:LINE:COL)
       (assertion_failed_at app/emacs/lib/ecaml/test/test_async.ml:LINE:COL))
      "should raise")) |}];
  return ()
;;

let%expect_test "raise in run_outside_async" =
  let%bind result =
    Monitor.try_with_or_error (fun () ->
      Async_ecaml.Private.run_outside_async [%here] (fun () ->
        raise_s [%sexp "Hello world"]))
  in
  print_s [%sexp (result : unit Or_error.t)];
  [%expect
    {|
    (Error (
      monitor.ml.Error "Hello world" (
        "<backtrace elided in test>" "Caught by monitor try_with_or_error"))) |}];
  return ()
;;

let%expect_test "assert_foreground in run_outside_async" =
  let%bind () =
    Deferred.create (fun background_job_complete ->
      Background.don't_wait_for [%here] (fun () ->
        let%bind () =
          Async_ecaml.Private.run_outside_async
            [%here]
            ~allowed_in_background:true
            (fun () ->
               require_does_raise [%here] ~hide_positions:true (fun () ->
                 Background.assert_foreground
                   [%here]
                   ~message:[%message "should raise"]))
        in
        Ivar.fill background_job_complete ();
        return ()))
  in
  [%expect
    {|
    ("Assertion failed -- running in background job"
     ((background_job_started_at app/emacs/lib/ecaml/test/test_async.ml:LINE:COL)
      (assertion_failed_at app/emacs/lib/ecaml/test/test_async.ml:LINE:COL))
     "should raise") |}];
  return ()
;;

let%expect_test "[run_outside_async ~allowed_in_background:false]" =
  let%bind () =
    Deferred.create (fun background_job_complete ->
      Background.don't_wait_for [%here] (fun () ->
        let%bind () =
          require_does_raise_async [%here] ~hide_positions:true (fun () ->
            Async_ecaml.Private.run_outside_async
              [%here]
              ~allowed_in_background:false
              (fun () -> ()))
        in
        Ivar.fill background_job_complete ();
        return ()))
  in
  [%expect
    {|
    ("Assertion failed -- running in background job"
     ((background_job_started_at app/emacs/lib/ecaml/test/test_async.ml:LINE:COL)
      (assertion_failed_at app/emacs/lib/ecaml/test/test_async.ml:LINE:COL))
     "[run_outside_async] called unsafely in background") |}];
  return ()
;;

let%expect_test "raise in [Background.don't_wait_for]" =
  let%bind () =
    require_does_not_raise_async [%here] (fun () ->
      let ready_to_raise = Ivar.create () in
      let background_job_ran =
        Deferred.create (fun background_job_ran ->
          Background.don't_wait_for [%here] (fun () ->
            let%bind () = Ivar.read ready_to_raise in
            Ivar.fill background_job_ran ();
            raise_s [%message "something bad happened"]))
      in
      Ivar.fill ready_to_raise ();
      background_job_ran)
  in
  [%expect
    {|
    ("background job raised" (job_created_at _) "something bad happened") |}];
  return ()
;;

module Test_enqueue_block_on_async = struct
  (* We need to allow nested [block_on_async] because [let%expect_test] uses
     [block_on_async], and inside a [let%expect_test] we want to test
     [schedule_foreground_block_on_async], which is implemented via [block_on_async], *)
  module Expect_test_config =
    Async_ecaml.Expect_test_config_allowing_nested_block_on_async

  let%expect_test "[schedule_foreground_block_on_async]" =
    let%bind () =
      Deferred.create (fun test_finished ->
        Background.don't_wait_for [%here] (fun () ->
          print_endline "background";
          require [%here] (Background.am_running_in_background ());
          Background.schedule_foreground_block_on_async [%here] (fun () ->
            print_endline "foreground";
            require [%here] (Background.am_running_in_foreground ());
            Ivar.fill test_finished ();
            return ());
          return ()))
    in
    [%expect {|
      background
      foreground |}];
    return ()
  ;;

  let%expect_test "[schedule_foreground_block_on_async] and [run_outside_async]" =
    let%bind () =
      Deferred.create (fun test_finished ->
        Background.don't_wait_for [%here] (fun () ->
          print_endline "background";
          require [%here] (Background.am_running_in_background ());
          Background.schedule_foreground_block_on_async [%here] (fun () ->
            print_endline "foreground";
            require [%here] (Background.am_running_in_foreground ());
            let%bind () =
              Async_ecaml.Private.run_outside_async [%here] (fun () ->
                print_endline "outside async";
                require [%here] (Background.am_running_in_foreground ()))
            in
            Ivar.fill test_finished ();
            return ());
          return ()))
    in
    [%expect {|
      background
      foreground
      outside async |}];
    return ()
  ;;

  let%expect_test "raise in [schedule_foreground_block_on_async]" =
    let%bind () =
      Deferred.create (fun test_finished ->
        let monitor = Monitor.create ~name:"test monitor" () in
        Monitor.detach_and_iter_errors monitor ~f:(fun exn ->
          print_s [%sexp (exn : exn)];
          Ivar.fill test_finished ());
        Background.don't_wait_for [%here] (fun () ->
          Background.Private.schedule_foreground_block_on_async
            [%here]
            ~raise_exceptions_to_monitor:monitor
            (fun () ->
               require [%here] (Background.am_running_in_foreground ());
               raise_s [%message "Raise an exception!"]);
          return ()))
    in
    [%expect
      {|
      (monitor.ml.Error "Raise an exception!" (
        "<backtrace elided in test>" "Caught by monitor test monitor")) |}];
    return ()
  ;;
end
