open! Core_kernel
open! Import
open! Minor_mode

let%expect_test "[is_enabled], [enable], [disable]" =
  let is_enabled () = print_s [%sexp (is_enabled view : bool)] in
  is_enabled ();
  [%expect {|
    false |}];
  enable view;
  is_enabled ();
  [%expect {|
    true |}];
  disable view;
  is_enabled ();
  [%expect {|
    false |}];
;;

let%expect_test "[abbrev]" =
  let is_enabled () = print_s [%sexp (is_enabled abbrev : bool)] in
  is_enabled ();
  [%expect {|
    false |}];
  enable abbrev;
  is_enabled ();
  [%expect {|
    true |}];
  disable abbrev;
  is_enabled ();
  [%expect {|
    false |}];
;;

let%expect_test "[read_only]" =
  let is_enabled () = print_s [%sexp (is_enabled read_only : bool)] in
  is_enabled ();
  [%expect {|
    false |}];
  enable read_only;
  is_enabled ();
  [%expect {|
    true |}];
  disable read_only;
  is_enabled ();
  [%expect {|
    false |}];
;;
