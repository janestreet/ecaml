open! Core_kernel
open! Async_kernel
open! Import
open! Documentation

let%expect_test "[substitute_command_keys]" =
  let test s = print_endline (substitute_command_keys s) in
  test {| \[find-file] |};
  [%expect {| C-x C-f |}];
  test {| \[substitute-command-keys] |};
  [%expect {| M-x substitute-command-keys |}];
  return ()
;;
