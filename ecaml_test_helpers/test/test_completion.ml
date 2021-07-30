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
      In this buffer, type RET to select the completion near point.

      Possible completions are:
      foo.tmp
      foobar.tmp |}];
    return ())
;;
