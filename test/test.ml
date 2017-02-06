open! Core
open! Async
open! Import

let%test "list reverse inline test" =
  List.rev [1; 2; 3] = [3; 2; 1]
;;

let%test "should raise" = does_raise (fun () -> List.find_exn [1; 2; 3] ~f:(Int.equal 4))

let%expect_test "list reverse expect test" =
  List.rev [1; 2; 3]
  |> [%sexp_of: int list]
  |> print_s;
  [%expect "(3 2 1)"];
;;

let%expect_test "Emacs math" =
  Expression.eval ("(+ 1 2)" |> Expression.of_string)
  |> Value.to_int
  |> printf "%d\n";
  [%expect {|
    3 |}];
;;
