open! Core
open! Async_kernel
open! Import
open! Form

let%expect_test "[eval]" =
  List.iter
    [ "13"; "(+ 1 4)"; "((lambda (x) (+ x 1)) 13)"; "(+ \"foo\")" ]
    ~f:(fun (string : string) ->
      let value = Or_error.try_with (fun () -> Blocking.eval_string string) in
      print_s [%message "" ~_:(string : string) "-->" ~_:(value : Value.t Or_error.t)]);
  [%expect
    {|
    (13 --> (Ok 13))
    ("(+ 1 4)" --> (Ok 5))
    ("((lambda (x) (+ x 1)) 13)" --> (Ok 14))
    ("(+ \"foo\")" --> (Error (wrong-type-argument (number-or-marker-p foo))))
    |}];
  return ()
;;

let%expect_test "[eval_string]" =
  let%bind v = eval_string "(+ 1 2)" in
  print_s [%sexp (v : Value.t)];
  [%expect {| 3 |}];
  return ()
;;

let test_read string =
  let form = Form.read string in
  print_s [%sexp (form : Form.t)]
;;

let%expect_test "[read]" =
  test_read "(+ 1 2)";
  [%expect {| (+ 1 2) |}];
  return ()
;;

let%expect_test "[read] with invalid syntax" =
  require_does_raise (fun () -> test_read ")");
  [%expect {| (invalid-read-syntax (")")) |}];
  return ()
;;

let%expect_test "[read] with trailing unclosed delimiter" =
  test_read "(+ 1 2) (";
  [%expect {| (+ 1 2) |}];
  return ()
;;

let%expect_test "[read] with trailing garbage" =
  require_does_raise (fun () -> test_read "(+ 1 2) a");
  [%expect
    {|
    ("Trailing data in string"
      (string            "(+ 1 2) a")
      (end_of_first_read 7))
    |}];
  return ()
;;

let%expect_test "[read] with trailing invalid syntax" =
  require_does_raise (fun () -> test_read "(+ 1 2)))");
  [%expect
    {|
    ("Raised while scanning for trailing data"
      (string            "(+ 1 2)))")
      (end_of_first_read 7)
      (exn (invalid-read-syntax (")"))))
    |}];
  return ()
;;
