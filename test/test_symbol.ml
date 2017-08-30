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

let%expect_test "[value_exn] raise" =
  let t = create ~name:"z" in
  show_raise (fun () -> value_exn t);
  [%expect {|
    (raised ("[Symbol.value_exn] of symbol with no value field" (symbol z))) |}];
;;

let%expect_test "[value] of unbound symbol" =
  print_s [%sexp (value (create ~name:"z") : Value.t option)];
  [%expect {|
    () |}];
;;

let%expect_test "[clear_value]" =
  let t = create ~name:"z" in
  let show () = print_s [%sexp (value t : Value.t option)] in
  show ();
  [%expect {|
    () |}];
  set_value t (13 |> Value.of_int_exn);
  show ();
  [%expect {|
    (13) |}];
  clear_value t;
  show ();
  [%expect {|
    () |}];
;;

let%expect_test "[set_value], [value_exn]" =
  let t = create ~name:"z" in
  set_value t (13 |> Value.of_int_exn);
  print_s [%sexp (value_exn t : Value.t)];
  [%expect {|
    13 |}];
;;

let%expect_test "[has_non_null_value]" =
  let t = create ~name:"zzz" in
  let show () = print_s [%sexp (has_non_null_value t : bool)] in
  show ();
  [%expect {|
    false |}];
  set_value t Value.nil;
  show ();
  [%expect {|
    false |}];
  set_value t Value.t;
  show ();
  [%expect {|
    true |}];
  set_value t (13 |> Value.of_int_exn);
  show ();
  [%expect {|
    true |}];
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

let%expect_test "[set_value_temporarily]" =
  let t = create ~name:"z" in
  let show () = print_s [%sexp (value_exn t : Value.t)] in
  set_value t (13 |> Value.of_int_exn);
  show ();
  [%expect {|
    13 |}];
  set_value_temporarily t (14 |> Value.of_int_exn) ~f:(fun () ->
    show ();
    [%expect {|
      14 |}]);
  show ();
  [%expect {|
    13 |}];
;;

let%expect_test "[set_value_temporarily] with an unbound symbol" =
  let t = create ~name:"z" in
  let show () = print_s [%sexp (value t : Value.t option)] in
  show ();
  [%expect {|
    () |}];
  set_value_temporarily t (13 |> Value.of_int_exn) ~f:(fun () ->
    show ();
    [%expect {|
      (13) |}]);
  show ();
  [%expect {|
    () |}];
;;

let%expect_test "[set_values_temporarily]" =
  let t1 = create ~name:"z1" in
  let t2 = create ~name:"z2" in
  let i = Value.of_int_exn in
  let show () =
    print_s [%message
      ""
        ~_:(value_exn t1 : Value.t)
        ~_:(value_exn t2 : Value.t)] in
  set_value t1 (i 13);
  set_value t2 (i 14);
  show ();
  [%expect {|
    (13 14) |}];
  set_values_temporarily [ t1, i 15; t2, i 16 ] ~f:(fun () ->
    show ();
    [%expect {|
      (15 16) |}]);
  show ();
  [%expect {|
    (13 14) |}];
;;
