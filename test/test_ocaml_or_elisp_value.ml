open! Core_kernel
open! Async_kernel
open! Import
module M = (val Ocaml_or_elisp_value.make Value.Type.int)
open M

let show t = print_s [%sexp (t : t)]

let%expect_test "[sexp_of_t]" =
  show (This 13);
  [%expect {| (This 13) |}];
  show (create_elisp 13);
  [%expect {| (Elisp 13) |}];
  return ()
;;

let%expect_test "[to_value]" =
  let to_value t = print_s [%sexp (to_value t : Value.t)] in
  to_value (This 13);
  [%expect {| 13 |}];
  to_value (create_elisp 13);
  [%expect {| 13 |}];
  return ()
;;
