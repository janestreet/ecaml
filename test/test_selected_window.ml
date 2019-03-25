open! Core
open! Import
open! Selected_window

let show_all () = print_s [%sexp (Window.all_in_selected_frame () : Window.t list)]
let show () = print_s [%sexp (get () : Window.t)]

let%expect_test "[get]" =
  show ();
  [%expect {| "#<window 1 on *scratch*>" |}]
;;

let%expect_test "[switch_to_buffer]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    switch_to_buffer (Current_buffer.get ());
    show ());
  [%expect {|
    "#<window 1 on *temp-buffer*>" |}]
;;

let%expect_test "[split_horizontally_exn] raise" =
  show_raise (fun () -> Selected_window.split_horizontally_exn ());
  [%expect
    {|
    (raised ("Window #<window 1 on *scratch*> too small for splitting")) |}]
;;

let%expect_test "[split_vertically_exn]" =
  Selected_window.split_vertically_exn ();
  show_all ();
  [%expect {|
    ("#<window 1 on *scratch*>" "#<window 4 on *scratch*>") |}]
;;

let window1, window4 =
  match Window.all_in_selected_frame () with
  | [ x; y ] -> x, y
  | [ w ] -> w, w
  | _ -> raise_s [%message [%here]]
;;

let%expect_test "[set]" =
  show ();
  [%expect {|
    "#<window 1 on *scratch*>" |}];
  set window4;
  show ();
  [%expect {|
    "#<window 4 on *scratch*>" |}]
;;

let%expect_test "[set_temporarily]" =
  show ();
  [%expect {|
    "#<window 4 on *scratch*>" |}];
  set_temporarily window1 ~f:show;
  [%expect {|
    "#<window 1 on *scratch*>" |}];
  show ();
  [%expect {|
    "#<window 4 on *scratch*>" |}]
;;

let%expect_test "[Blocking.find_file]" =
  Blocking.find_file "test_selected_window.ml";
  print_endline
    (String.sub ~pos:0 ~len:10 (Current_buffer.contents () |> Text.to_utf8_bytes));
  [%expect {|
    open! Core |}];
  Buffer.kill (Current_buffer.get ())
;;

module Test_async = struct
  open! Async
  open! Async_ecaml

  let%expect_test "[find_file]" =
    let%bind () = find_file "test_selected_window.ml" in
    print_endline
      (String.sub ~pos:0 ~len:10 (Current_buffer.contents () |> Text.to_utf8_bytes));
    let%map () = [%expect {|
    open! Core |}] in
    Buffer.kill (Current_buffer.get ())
  ;;
end
