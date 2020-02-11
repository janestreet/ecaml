open! Core_kernel
open! Async_kernel
open! Import
open! Print

let%expect_test "[length]" =
  print_s [%sexp (Current_buffer.value_exn Print.length : int option)];
  [%expect {| () |}];
  return ()
;;

let%expect_test "[level]" =
  print_s [%sexp (Current_buffer.value_exn Print.level : int option)];
  [%expect {| () |}];
  return ()
;;
