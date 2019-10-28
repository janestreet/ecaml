open! Core_kernel
open! Async_kernel
open! Import

let%test "list reverse inline test" =
  [%equal: int list] (List.rev [ 1; 2; 3 ]) [ 3; 2; 1 ]
;;

let%test "should raise" =
  does_raise (fun () -> List.find_exn [ 1; 2; 3 ] ~f:(Int.equal 4))
;;

let%expect_test "list reverse expect test" =
  List.rev [ 1; 2; 3 ] |> [%sexp_of: int list] |> print_s;
  [%expect "(3 2 1)"];
  return ()
;;

let%expect_test "Emacs math" =
  let%bind v = Form.eval_string "(+ 1 2)" in
  v |> Value.to_int_exn |> printf "%d\n";
  [%expect {| 3 |}];
  return ()
;;
