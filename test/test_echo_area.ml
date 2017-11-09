open! Core_kernel
open! Import
open! Echo_area

(* Expect tests run with [emacs -batch], which causes messages to go to stderr in addition
   to the *Messages* buffer.  Since ppx_expect collects output on stderr, we get a second
   copy of the output. *)
let show () =
  let buffer = Buffer.find_or_create ~name:"*Messages*" in
  Current_buffer.set_temporarily buffer ~f:(fun () ->
    print_string (Current_buffer.contents () |> Text.to_utf8_bytes));
  Buffer.kill buffer;
;;

let%expect_test "[message_value]" =
  message_value (13 |> Value.of_int_exn);
  show ();
  [%expect {|
    13
    13 |}]
;;

let%expect_test "[message]" =
  message "foobar";
  show ();
  [%expect {|
    foobar
    foobar |}]
;;

let%expect_test "[messagef]" =
  messagef "%d" 13;
  show ();
  [%expect {|
    13
    13 |}]
;;

let%expect_test "[message_s]" =
  message_s [%message "foobar" ~_:(13 : int)];
  show ();
  [%expect {|
   (foobar 13)
   (foobar 13) |}]
;;

(* This test has only a single instance of the output, which shows that [inhibit_messages]
   prevents [message] from displaying the data, but that it is still put in the *Messages*
   buffer. *)
let%expect_test "[inhibit_messages]" =
  inhibit_messages (fun () ->
    message "hello");
  show ();
  [%expect {|
    hello |}];
;;
