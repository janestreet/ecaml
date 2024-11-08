open! Core
open! Async_kernel
open! Import
open! Selected_window

let show_all () = print_s [%sexp (Frame.window_list () : Window.t list)]
let show () = print_s [%sexp (get () : Window.t)]

module%test _ = struct
  let%expect_test "[get]" =
    show ();
    [%expect {| "#<window 1 on *scratch*>" |}];
    return ()
  ;;

  let%expect_test "[pop_to_buffer]" =
    Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
      Blocking.pop_to_buffer (Current_buffer.get ());
      show ());
    [%expect {| "#<window 1 on  *temp*>" |}];
    return ()
  ;;

  let%expect_test "[split_horizontally_exn] raise" =
    show_raise (fun () ->
      Current_buffer.set_value_temporarily
        Sync
        Var.Wrap.("window-min-width" <: int)
        45
        ~f:(fun () -> Selected_window.split_horizontally_exn ()));
    [%expect {| (raised ("Window #<window 1 on *scratch*> too small for splitting")) |}];
    return ()
  ;;

  let%expect_test "[split_vertically_exn]" =
    Selected_window.split_vertically_exn ();
    show_all ();
    [%expect {| ("#<window 1 on *scratch*>" "#<window 4 on *scratch*>") |}];
    return ()
  ;;

  let%expect_test "[set]" =
    let window1, window4 =
      match Frame.window_list () with
      | [ x; y ] -> x, y
      | _ -> raise_s [%message [%here]]
    in
    show ();
    [%expect {| "#<window 1 on *scratch*>" |}];
    set window4;
    show ();
    [%expect {| "#<window 4 on *scratch*>" |}];
    set_temporarily Sync window1 ~f:show;
    [%expect {| "#<window 1 on *scratch*>" |}];
    show ();
    [%expect {| "#<window 4 on *scratch*>" |}];
    let%bind () =
      set_temporarily Async window1 ~f:(fun () ->
        let%map () = Clock.after (sec 0.001) in
        show ())
    in
    [%expect {| "#<window 1 on *scratch*>" |}];
    show ();
    [%expect {| "#<window 4 on *scratch*>" |}];
    return ()
  ;;

  let%expect_test "[find_file]" =
    let%bind () = find_file "test_selected_window.ml" in
    print_endline
      (String.sub ~pos:0 ~len:10 (Current_buffer.contents () |> Text.to_utf8_bytes));
    [%expect {| open! Core |}];
    Buffer.kill (Current_buffer.get ())
  ;;
end
