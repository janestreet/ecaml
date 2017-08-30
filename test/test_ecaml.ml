open! Core_kernel
open! Import

let%expect_test "[inhibit_read_only]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    Current_buffer.Minor_mode.(enable read_only);
    show_raise (fun () -> Point.insert "foo");
    [%expect {|
      (raised (signal (symbol buffer-read-only) (data (#<buffer *temp-buffer*>)))) |}];
    inhibit_read_only (fun () -> Point.insert "foo");
    print_s [%sexp (Current_buffer.contents () : Text.t)];
    [%expect {|
      foo |}]);
;;
