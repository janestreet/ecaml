open! Core
open! Import
open! Selected_window

let show_all () = print_s [%sexp (Window.all_in_selected_frame () : Window.t list)]

let show () = print_s [%sexp (get () : Window.t)]

let%expect_test "[get]" =
  show ();
  [%expect {| "#<window 1 on *scratch*>" |}];
;;

let%expect_test "[switch_to_buffer]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    switch_to_buffer (Current_buffer.get ());
    show ());
  [%expect {|
    "#<window 1 on *temp-buffer*>" |}];
;;

let%expect_test "[split_horizontally_exn] raise" =
  show_raise (fun () ->
    Selected_window.split_horizontally_exn ());
  [%expect {|
    (raised ("Window #<window 1 on *scratch*> too small for splitting")) |}]
;;

let%expect_test "[split_vertically_exn]" =
  Selected_window.split_vertically_exn ();
  show_all ();
  [%expect {|
    ("#<window 1 on *scratch*>" "#<window 4 on *scratch*>") |}]
;;

let%expect_test "[set]" =
  show ();
  [%expect {|
    "#<window 1 on *scratch*>" |}];
  set (List.nth_exn (Window.all_in_selected_frame ()) 1);
  show ();
  [%expect {|
    "#<window 4 on *scratch*>" |}];
;;

let%expect_test "[find_file]" =
  find_file "test_selected_window.ml";
  print_endline (String.sub ~pos:0 ~len:10 (Current_buffer.contents () |> Text.to_utf8_bytes));
  [%expect {|
    open! Core |}];
  Buffer.kill (Current_buffer.get ());
;;
