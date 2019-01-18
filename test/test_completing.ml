open! Core_kernel
open! Async_kernel
open! Import
open! Completing

let collection = Collection.This [ "foo"; "bar"; "baz" ]
let prompt = ""

let%expect_test "completing_read" =
  with_input_macro "bar TAB RET" (fun () ->
    let%bind response = read () ~collection ~prompt in
    print_s [%sexp (response : string)];
    return ());
  [%expect {| bar |}]
;;

let%expect_test "completing_read" =
  with_input_macro "f TAB RET" (fun () ->
    let%bind response = read () ~collection ~prompt in
    print_s [%sexp (response : string)];
    return ());
  [%expect {|
    foo |}]
;;

let%expect_test "completing_read_multiple" =
  with_input_macro "bar,f TAB RET" (fun () ->
    let%bind response = read_multiple () ~collection ~prompt in
    print_s [%sexp (response : string list)];
    return ());
  [%expect {| (bar foo) |}]
;;

let%expect_test "completing_read_multiple in the other order" =
  with_input_macro "f TAB ,bar RET" (fun () ->
    let%bind response = read_multiple () ~collection ~prompt in
    print_s [%sexp (response : string list)];
    return ());
  [%expect {| (foo bar) |}]
;;
