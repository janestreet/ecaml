open! Core_kernel
open! Async_kernel
open! Import
open! System

let%expect_test "[setenv], [getenv], [setenv_temporarily]" =
  let var = "ZZZ" in
  let setenv value = setenv ~var ~value:(Some value) in
  let getenv () = print_s [%sexp (getenv ~var : string option)] in
  setenv "a";
  getenv ();
  [%expect {| (a) |}];
  setenv "b";
  getenv ();
  [%expect {| (b) |}];
  setenv_temporarily Sync [ { var; value = "c" } ] ~f:(fun () ->
    getenv ();
    [%expect {| (c) |}];
    setenv "d";
    getenv ();
    [%expect {| (d) |}]);
  getenv ();
  [%expect {| (b) |}];
  return ()
;;

let%expect_test "[noninteractive]" =
  print_s [%sexp (Current_buffer.value_exn noninteractive : bool)];
  [%expect {| true |}];
  return ()
;;
