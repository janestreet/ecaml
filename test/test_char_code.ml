open! Core_kernel
open! Import
open! Char_code

let show t = print_s [%sexp (t : t)]

let%expect_test "[min_value]" =
  print_s [%sexp (min_value : t)];
  [%expect {|
    0 |}];
;;

let%expect_test "[max_value]" =
  print_s [%sexp (max_value : t)];
  [%expect {|
    4194303 |}];
;;

let%expect_test "[of_char_exn]" =
  show ('a' |> of_char_exn);
  [%expect {|
    97 |}];
;;

let%expect_test "[of_char_exn] raise" =
  show_raise (fun () -> of_char_exn '\128');
  [%expect {|
    (raised (
      "[Char_code.of_char_exn] got non-ASCII character"
      (char "\128")
      (code 128))) |}];
;;

let%expect_test "[of_int_exn], [to_int]" =
  let t = of_int_exn 97 in
  print_s [%sexp (t : t)];
  [%expect {|
    97 |}];
  print_s [%sexp (to_int t : int)];
  [%expect {|
    97 |}];
;;

let%expect_test "[of_int_exn] raise" =
  show_raise (fun () -> of_int_exn ((min_value |> to_int) - 1));
  [%expect {|
    (raised ("[char-code]'s [of_value_exn] got value not in subtype" -1)) |}];
  show_raise (fun () -> of_int_exn ((max_value |> to_int) + 1));
  [%expect {|
    (raised ("[char-code]'s [of_value_exn] got value not in subtype" 4194304)) |}];
;;
