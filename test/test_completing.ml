open! Core
open! Async_kernel
open! Import
open! Completing

let collection = [ "foo"; "bar"; "baz" ]
let prompt_no_colon = ""

let%expect_test "completing_read" =
  let%bind () =
    with_input_macro "bar TAB RET" (fun () ->
      let%bind response =
        read ~prompt_no_colon ~collection ~history:Minibuffer.history ()
      in
      print_s [%sexp (response : string)];
      return ())
  in
  [%expect
    {|
    [minibuffer-message] Sole completion

    bar
    |}];
  return ()
;;

let%expect_test "completing_read" =
  let%bind () =
    with_input_macro "f TAB RET" (fun () ->
      let%bind response =
        read ~prompt_no_colon ~collection ~history:Minibuffer.history ()
      in
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
        read_multiple ~prompt_no_colon ~collection ~history:Minibuffer.history ()
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
        read_multiple ~prompt_no_colon ~collection ~history:Minibuffer.history ()
      in
      print_s [%sexp (response : string list)];
      return ())
  in
  [%expect {| (foo bar) |}];
  return ()
;;
