open! Core_kernel
open! Async_kernel
open! Import
open! Comment

let%expect_test "options" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    Funcall.("c-mode" <: nullary @-> return nil) ();
    let show var sexp_of_a = print_s [%sexp (Current_buffer.value_exn var : a)] in
    show start String.sexp_of_t;
    [%expect {| "/* " |}];
    show end_ String.sexp_of_t;
    [%expect {| " */" |}];
    show multi_line Bool.sexp_of_t;
    [%expect {| true |}];
    return ())
;;
