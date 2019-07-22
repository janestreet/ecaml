open! Core_kernel
open! Async_kernel
open! Import
open! Plist

let show t = print_s [%sexp (t : t)]

let%expect_test "[of_symbol]" =
  let x = "x" |> Symbol.intern in
  show (of_symbol x);
  [%expect
    {|
    (custom-group (
      (x-dnd-test-function custom-variable)
      (x-dnd-types-alist   custom-variable)
      (x-dnd-known-types   custom-variable)
      (x-gtk-stock-map     custom-variable)
      (icon-map-list       custom-variable))) |}];
  return ()
;;

let%expect_test "[get] and [set]" =
  let x = "x" |> Symbol.intern in
  let prop = "prop" |> Symbol.intern in
  let t = of_symbol x in
  print_s [%sexp (get t prop : Value.t option)];
  [%expect {| () |}];
  set t prop (13 |> Value.of_int_exn);
  print_s [%sexp (get t prop : Value.t option)];
  [%expect {| (13) |}];
  return ()
;;
