open! Core
open! Async
open! Import

let%expect_test "[eval]" =
  List.iter
    [ "13"
    ; "(+ 1 4)"
    ; "((lambda (x) (+ x 1)) 13)"
    ; "(+ \"foo\")"
    ]
    ~f:(fun (string : string) ->
      let value =
        Or_error.try_with (fun () -> Expression.eval (string |> Expression.of_string))
      in
      print_s [%message "" ~_:(string : string) "-->" ~_:(value : Value.t Or_error.t)]);
  [%expect {|
    (13 --> (Ok 13))
    ("(+ 1 4)" --> (Ok 5))
    ("((lambda (x) (+ x 1)) 13)" --> (Ok 14))
    ("(+ \"foo\")" --> (
      Error (signal (symbol wrong-type-argument) (data (number-or-marker-p foo))))) |}];
;;
