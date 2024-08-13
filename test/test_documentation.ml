open! Core
open! Async_kernel
open! Import
open! Documentation

let%expect_test "[substitute_command_keys]" =
  let test s =
    print_s [%sexp (substitute_command_keys (Text.of_utf8_bytes s) : Text.t)]
  in
  test {|The key binding for `find-file' is \[find-file].|};
  [%expect
    {|
    ("The key binding for `find-file' is C-x C-f."
     35
     42
     (font-lock-face help-key-binding face help-key-binding))
    |}];
  test {|\[substitute-command-keys] does not have a key binding by default.|};
  [%expect
    {|
    ("M-x substitute-command-keys does not have a key binding by default."
     0
     27
     (font-lock-face help-key-binding face help-key-binding))
    |}];
  return ()
;;
