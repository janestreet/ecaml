open! Core
open! Async_kernel
open! Import
open! Mode_line

let%expect_test _ =
  let format = Current_buffer.get_buffer_local Format.in_buffer in
  print_s [%sexp (format : Format.t)];
  [%expect
    {|
    (%e mode-line-front-space
      (:propertize
        (""
         mode-line-mule-info
         mode-line-client
         mode-line-modified
         mode-line-remote
         mode-line-window-dedicated)
        display
        (min-width (6.0)))
      mode-line-frame-identification
      mode-line-buffer-identification
      "   "
      mode-line-position
      (project-mode-line project-mode-line-format)
      (vc-mode           vc-mode)
      "  "
      mode-line-modes
      mode-line-misc-info
      mode-line-end-spaces)
    |}];
  (* Emacs, when run non-interactively, makes [format-mode-line] return the empty
     string.  So we can only test that we're calling it. *)
  print_s [%sexp (text format : Text.t)];
  [%expect {| "" |}];
  return ()
;;
