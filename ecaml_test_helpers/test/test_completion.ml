open! Core
open! Async_kernel
open! Import

let%expect_test "[press_and_show_prompt] shows completions" =
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

let%expect_test "failure to complete on RET, messages" =
  let%bind () = press_and_show_minibuffer "C-x k zzzzz RET" in
  [%expect
    {|
    [minibuffer-message] No match

    Kill buffer (default *scratch*): zzzzz
    
    |}];
  return ()
;;

let%expect_test "errors in minibuffer do not cause hang" =
  let%bind () = press_and_show_minibuffer "C-x k zzzzz C-f" in
  [%expect {| ("Command errored in minibuffer" "End of buffer") |}];
  return ()
;;
