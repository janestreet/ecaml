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
        let basename = Filename.nondirectory tmpdir in
        print_string (replace [%expect.output] ~pattern:basename ~with_:"BASENAME/");
        [%expect
          {|
          Save file TMPDIR/BASENAME/foo.txt? (y, n, !, ., q, M-~, C-r, C-f, d or C-h)

          Save file TMPDIR/BASENAME/foo.txt? (y, n, !, ., q, M-~, C-r, C-f, d or C-h) C-]
          |}];
        return ()))
;;

let%expect_test "[press] with [save-some-buffers] reports the minibuffer prompt" =
  with_temp_dir (fun tmpdir ->
    Current_buffer.set_buffer_local_temporarily
      Async
      Current_buffer.directory
      (Some tmpdir)
      ~f:(fun () ->
        let%bind () = press "C-x C-f foo.txt RET asdf" in
        [%expect {| asdf█ |}];
        let basename = Filename.nondirectory tmpdir in
        (* Validate that quitting explicitly works, and shows the prompt only once. *)
        let%bind () = require_does_raise_async (fun () -> press "C-x s C-]") in
        print_string (replace [%expect.output] ~pattern:basename ~with_:"BASENAME/");
        [%expect
          {|
          Save file TMPDIR/BASENAME/foo.txt? (y, n, !, ., q, M-~, C-r, C-f, d or C-h) C-]
          quit
          |}];
        let%bind () = require_does_raise_async (fun () -> press "C-x s") in
        print_string (replace [%expect.output] ~pattern:basename ~with_:"BASENAME/");
        [%expect
          {|
          ("Minibuffer open"
           ((prompt
             ("Save file TMPDIR/BASENAME/foo.txt? (y, n, !, ., q, M-~, C-r, C-f, d or C-h) "))))
          quit
          |}];
        return ()))
;;
