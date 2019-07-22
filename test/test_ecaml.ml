open! Core_kernel
open! Async_kernel
open! Import

let%expect_test "[inhibit_read_only]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    Minor_mode.(enable read_only);
    show_raise (fun () -> Point.insert "foo");
    [%expect {| (raised (buffer-read-only ("#<buffer *temp-buffer*>"))) |}];
    inhibit_read_only Sync (fun () -> Point.insert "foo");
    print_s [%sexp (Current_buffer.contents () : Text.t)];
    [%expect {| foo |}];
    return ())
;;

let%expect_test "[inhibit_read_only]" =
  Current_buffer.set_temporarily_to_temp_buffer Async (fun () ->
    Minor_mode.(enable read_only);
    show_raise (fun () -> Point.insert "foo");
    [%expect {| (raised (buffer-read-only ("#<buffer *temp-buffer*>"))) |}];
    let%bind () =
      inhibit_read_only Async (fun () ->
        let%map () = Clock.after (sec 0.001) in
        Point.insert "foo")
    in
    print_s [%sexp (Current_buffer.contents () : Text.t)];
    [%expect {| foo |}];
    return ())
;;
