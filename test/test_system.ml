open! Core
open! Async_kernel
open! Import
open! System

let%expect_test "[setenv], [getenv], [setenv_temporarily]" =
  let key = "ZZZ" in
  let setenv value = setenv ~key ~data:(Some value) in
  let getenv () = print_s [%sexp (getenv key : string option)] in
  setenv "a";
  getenv ();
  [%expect {| (a) |}];
  setenv "b";
  getenv ();
  [%expect {| (b) |}];
  setenv_temporarily
    Sync
    [ { key; data = "c" } ]
    ~f:(fun () ->
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
