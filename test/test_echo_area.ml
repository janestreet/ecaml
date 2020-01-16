open! Core_kernel
open! Async_kernel
open! Import
open! Echo_area

(* Expect tests run with [emacs -batch], which causes messages to go to stderr in addition
   to the *Messages* buffer.  Since ppx_expect collects output on stderr, we get a second
   copy of the output. *)
let show () =
  let buffer = Buffer.find_or_create ~name:"*Messages*" in
  Current_buffer.set_temporarily Sync buffer ~f:(fun () ->
    print_string (Current_buffer.contents () |> Text.to_utf8_bytes));
  Buffer.Blocking.kill buffer
;;

let%expect_test "[message_s] of a value" =
  message_s [%sexp (13 |> Value.of_int_exn : Value.t)];
  show ();
  [%expect {|
    13
    13 |}];
  return ()
;;

let%expect_test "[message]" =
  message "foobar";
  show ();
  [%expect {|
    foobar
    foobar |}];
  return ()
;;

let%expect_test "[messagef]" =
  messagef "%d" 13;
  show ();
  [%expect {|
    13
    13 |}];
  return ()
;;

let%expect_test "[message_s] of an atom" =
  message_s [%message "foo bar"];
  show ();
  [%expect {|
    foo bar
    foo bar |}];
  return ()
;;

let%expect_test "[message_s]" =
  message_s [%message "foobar" ~_:(13 : int)];
  show ();
  [%expect {|
   (foobar 13)
   (foobar 13) |}];
  return ()
;;

let%expect_test "[message_text]" =
  message_text (Text.colorize (Text.of_utf8_bytes "hello") ~color:Color.red);
  show ();
  [%expect {|
    hello
    hello |}];
  return ()
;;

let%expect_test "[message ~echo:false]" =
  message "foo" ~echo:false;
  [%expect {| |}];
  show ();
  [%expect {| foo |}];
  return ()
;;

let%expect_test "[inhibit_messages]" =
  inhibit_messages Sync (fun () -> message "hello");
  [%expect {| |}];
  show ();
  [%expect {| hello |}];
  return ()
;;

let%expect_test "[inhibit_messages] in background job" =
  Deferred.create (fun ivar ->
    Background.don't_wait_for [%here] (fun () ->
      inhibit_messages Sync (fun () -> message "hello");
      [%expect {||}];
      show ();
      [%expect {| hello |}];
      let%bind () =
        show_raise_async ~hide_positions:true (fun () ->
          inhibit_messages Async (fun () ->
            message "world";
            return ()))
      in
      [%expect
        {|
        (raised (
          "Assertion failed -- running in background job"
          ((background_job_started_at
            app/emacs/lib/ecaml/test/test_echo_area.ml:LINE:COL)
           (assertion_failed_at app/emacs/lib/ecaml/src/echo_area.ml:LINE:COL))
          "Echo_area.inhibit_messages called asynchronously in background job")) |}];
      Ivar.fill ivar ();
      return ()))
;;
