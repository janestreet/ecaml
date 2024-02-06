open! Core
open! Async_kernel
open! Import

let%expect_test "[press_and_show_minibuffer] with [save-some-buffers] shows prompt \
                 multiple times"
  =
  with_temp_dir (fun tmpdir ->
    Current_buffer.set_buffer_local_temporarily
      Async
      Current_buffer.directory
      (Some tmpdir)
      ~f:(fun () ->
      let%bind () = press "C-x C-f foo.txt RET asdf" in
      [%expect {| asdf█ |}];
      (* It would be nicer if this only showed the prompt once, but it's not a huge
           deal. *)
      let%bind () = press_and_show_minibuffer "C-x s" in
      print_string (replace [%expect.output] ~pattern:tmpdir ~with_:"$TMPDIR/");
      [%expect
        {|
          Save file $TMPDIR/foo.txt? (y, n, !, ., q, C-r, C-f, d or C-h)
          Save file $TMPDIR/foo.txt? (y, n, !, ., q, C-r, C-f, d or C-h) C-u
          Type C-h for help.
          Save file $TMPDIR/foo.txt? (y, n, !, ., q, C-r, C-f, d or C-h)
          Save file $TMPDIR/foo.txt? (y, n, !, ., q, C-r, C-f, d or C-h) <f14>
          Type C-h for help.
          Save file $TMPDIR/foo.txt? (y, n, !, ., q, C-r, C-f, d or C-h)
          Save file $TMPDIR/foo.txt? (y, n, !, ., q, C-r, C-f, d or C-h) C-]
           |}];
      return ()))
;;

let%expect_test "[press] with [save-some-buffers] times out instead of showing prompt" =
  with_temp_dir (fun tmpdir ->
    Current_buffer.set_buffer_local_temporarily
      Async
      Current_buffer.directory
      (Some tmpdir)
      ~f:(fun () ->
      let%bind () = press "C-x C-f foo.txt RET asdf" in
      [%expect {| asdf█ |}];
      let%bind () = require_does_raise_async [%here] (fun () -> press "C-x s") in
      print_string (replace [%expect.output] ~pattern:tmpdir ~with_:"$TMPDIR/");
      [%expect
        {|
          Save file $TMPDIR/foo.txt? (y, n, !, ., q, C-r, C-f, d or C-h)
          Save file $TMPDIR/foo.txt? (y, n, !, ., q, C-r, C-f, d or C-h) <f13>
          ("Minibuffer open" ((prompt ())))
          (user-error ("No recursive edit is in progress")) |}];
      return ()))
;;
