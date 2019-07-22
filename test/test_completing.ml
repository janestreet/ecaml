open! Core_kernel
open! Async_kernel
open! Import
open! Completing

let collection = Collection.This [ "foo"; "bar"; "baz" ]
let prompt = ""

let%expect_test "completing_read" =
  let%bind () =
    with_input_macro "bar TAB RET" (fun () ->
      let%bind response = read ~prompt ~collection ~history:Minibuffer.history () in
      print_s [%sexp (response : string)];
      return ())
  in
  [%expect {| bar |}];
  return ()
;;

let%expect_test "completing_read" =
  let%bind () =
    with_input_macro "f TAB RET" (fun () ->
      let%bind response = read ~prompt ~collection ~history:Minibuffer.history () in
      print_s [%sexp (response : string)];
      return ())
  in
  [%expect {| foo |}];
  return ()
;;

let%expect_test "completing_read_multiple" =
  let%bind () =
    with_input_macro "bar,f TAB RET" (fun () ->
      let%bind response =
        read_multiple ~prompt ~collection ~history:Minibuffer.history ()
      in
      print_s [%sexp (response : string list)];
      return ())
  in
  [%expect {| (bar foo) |}];
  return ()
;;

let%expect_test "completing_read_multiple in the other order" =
  let%bind () =
    with_input_macro "f TAB ,bar RET" (fun () ->
      let%bind response =
        read_multiple ~prompt ~collection ~history:Minibuffer.history ()
      in
      print_s [%sexp (response : string list)];
      return ())
  in
  [%expect {| (foo bar) |}];
  return ()
;;
