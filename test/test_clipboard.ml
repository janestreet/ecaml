open! Core_kernel
open! Async_kernel
open! Import
open! Clipboard

let%expect_test "[kill_new], [yank_at_point]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert "foo\n";
    kill_new ("bar\n" |> Text.of_utf8_bytes);
    yank_at_point ();
    [%expect {| Mark set |}];
    yank_at_point ();
    [%expect {| Mark set |}];
    print_endline (Current_buffer.contents () |> Text.to_utf8_bytes);
    [%expect {|
      foo
      bar
      bar |}]);
  return ()
;;
