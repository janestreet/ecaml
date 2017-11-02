open! Core_kernel
open! Import
open! Var

let%expect_test "[default_value_exn] raise" =
  show_raise (fun () -> default_value_exn (int_var "z"));
  [%expect {|
    (raised (void-variable (z))) |}];
;;

let%expect_test "[default_value_exn]" =
  let t = int_var "z" in
  Current_buffer.set_value t 13;
  print_s [%sexp (default_value_exn t : int)];
  [%expect {|
    13 |}];
;;

let%expect_test "[set_default_value]" =
  let t = int_var "z" in
  set_default_value t 13;
  print_s [%sexp (default_value_exn t : int)];
  [%expect {|
    13 |}];
;;

let%expect_test "[default_value_is_defined]" =
  let t = int_var "z" in
  print_s [%sexp (default_value_is_defined t : bool)];
  [%expect {|
    false |}];
  Current_buffer.set_value t 13;
  print_s [%sexp (default_value_is_defined t : bool)];
  [%expect {|
    true |}];
;;

let%expect_test "[set_default_value]" =
  let t = int_var "z" in
  print_s [%sexp (default_value_is_defined t : bool)];
  [%expect {|
    false |}];
  set_default_value t 13;
  print_s [%sexp (default_value_is_defined t : bool)];
  [%expect {|
    true |}];
;;

let%expect_test "[make_buffer_local_always]" =
  let t = int_var "z" in
  make_buffer_local_always t;
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    print_s [%sexp (Current_buffer.is_buffer_local t : bool)];
    [%expect {|
      false |}];
    Current_buffer.set_value t 13;
    print_s [%sexp (Current_buffer.is_buffer_local t : bool)];
    [%expect {|
      true |}]);
;;
