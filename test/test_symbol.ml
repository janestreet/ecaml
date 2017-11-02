open! Core_kernel
open! Import
open! Symbol

let require = Expect_test_helpers_kernel.require

let%expect_test "[intern] with same name returns equal symbols" =
  require [%here] (equal (intern "foo") (intern "foo"));
;;

let%expect_test "[intern] with different names returns unequal symbols" =
  require [%here] (not (equal (intern "foo") (intern "bar")));
;;

let%expect_test "[create] with same name returns unequal symbols" =
  require [%here] (not (equal (create ~name:"foo") (create ~name:"foo")));
;;

let%expect_test "[name]" =
  print_s [%sexp (name (intern "foo") : string)];
  [%expect {|
    foo |}];
;;

let%expect_test "[function_exn] raise" =
  let t = create ~name:"z" in
  show_raise (fun () -> function_exn t);
  [%expect {|
    (raised ("[Symbol.function_exn] of symbol with no function field" (symbol z))) |}];
;;

let%expect_test "[set_function], [function_exn]" =
  let t = create ~name:"z" in
  set_function t (13 |> Value.of_int_exn);
  print_s [%sexp (function_exn t : Value.t)];
  [%expect {|
    13 |}];
;;
