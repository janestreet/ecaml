open! Core_kernel
open! Import
open! Echo_area

let restore_stderr = unstage (ignore_stderr ())

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
    13 |}]
;;

let%expect_test "[message]" =
  message "foobar";
  show ();
  [%expect {|
    foobar |}]
;;

let%expect_test "[messagef]" =
  messagef "%d" 13;
  show ();
  [%expect {|
    13 |}]
;;

let%expect_test "[message_s]" =
  message_s [%message "foobar" ~_:(13 : int)];
  show ();
  [%expect {|
   (foobar 13) |}]
;;

let () = restore_stderr ()

(* This test is after [restore_stderr] so that it shows that [inhibit_messages]
   prevents [message] from outputting. *)
let%expect_test "[inhibit_messages]" =
  inhibit_messages (fun () ->
    message "hello");
  show ();
  [%expect {|
    hello |}];
;;
