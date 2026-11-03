open! Core
open! Async_kernel
open! Import

let%expect_test "[press_and_show_minibuffer] shows completions and handles its final quit"
  =
  within_temp_dir (fun () ->
    File.ensure_exists "foo.tmp";
    File.ensure_exists "foobar.tmp";
    let%bind () =
      press_and_show_minibuffer "C-x C-f ./foo TAB TAB" ~show_contents:false
    in
    [%expect
      {|
      Making completion list...



      Find file:

      Contents of *Completions* buffer:
      Type M-RET on a completion to select it.
      Type M-<down> or M-<up> to move point between completions.

      2 possible completions:
      foo.tmp
      foobar.tmp
      |}];
    return ())
;;

let%expect_test "[press_and_show_minibuffer] reports failure to complete on RET" =
  let%bind () = press_and_show_minibuffer "C-x k zzzzz RET" in
  [%expect
    {|
    [minibuffer-message] No match



    Kill buffer (default *scratch*): zzzzz

    
    |}];
  return ()
;;

let%expect_test "[press_and_show_minibuffer] propagates errors before opening the \
                 minibuffer"
  =
  let buffer = Buffer.create ~name:"temp" in
  Window.set_buffer_exn (Selected_window.get ()) buffer;
  let%bind () =
    require_does_raise_async (fun () -> press_and_show_minibuffer "C-x C-f")
  in
  [%expect
    {|
    ("do not execute a key sequence when the current_buffer and selected_buffer are different -- [execute-kbd-macro] sets the current_buffer to the selected_buffer"
     (current_buffer  "#<buffer *scratch*>")
     (selected_buffer "#<buffer temp>"))
    |}];
  (* Put the window back on the current buffer, so later tests can execute keys. *)
  Selected_window.pop_to_buffer (Current_buffer.get ())
;;

let%expect_test "[press_and_show_minibuffer] reports minibuffer errors without hanging" =
  let%bind () = press_and_show_minibuffer "C-x k zzzzz C-f" in
  [%expect {| ("Command errored in minibuffer" "End of buffer") |}];
  return ()
;;
