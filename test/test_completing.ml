open! Core_kernel
open! Import
open! Completing

let collection =
  [ "foo"
  ; "bar"
  ; "baz"
  ]

let prompt = ""

let%expect_test "completing_read" =
  with_input_macro "bar TAB RET" (fun () ->
    print_s [%sexp (read () ~collection ~prompt : string)]);
  [%expect {| bar |}];
;;

let%expect_test "completing_read" =
  with_input_macro "f TAB RET" (fun () ->
    print_s [%sexp (read () ~collection ~prompt : string)]);
  [%expect {| foo |}];
;;

let%expect_test "completing_read_multiple" =
  with_input_macro "bar,f TAB RET" (fun () ->
    print_s [%sexp (read_multiple () ~collection ~prompt : string list)]);
  [%expect {| (bar foo) |}];
;;

let%expect_test "completing_read_multiple in the other order" =
  with_input_macro "f TAB ,bar RET" (fun () ->
    print_s [%sexp (read_multiple () ~collection ~prompt : string list)]);
  [%expect {| (foo bar) |}];
;;
