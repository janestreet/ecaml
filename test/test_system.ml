open! Core_kernel
open! Import
open! System

let%expect_test "[setenv], [getenv], [setenv_temporarily]" =
  let var = "ZZZ" in
  let setenv value = setenv ~var ~value:(Some value) in
  let getenv () = print_s [%sexp (getenv ~var : string option)] in
  setenv "a";
  getenv ();
  [%expect {|
    (a) |}];
  setenv "b";
  getenv ();
  [%expect {|
    (b) |}];
  setenv_temporarily [ { var; value = "c" } ] ~f:(fun () ->
    getenv ();
    [%expect {|
      (c) |}];
    setenv "d";
    getenv ();
    [%expect {|
      (d) |}]);
  getenv ();
  [%expect {|
    (b) |}];
;;
