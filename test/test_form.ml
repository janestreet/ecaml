open! Core_kernel
open! Import
open! Form

let%expect_test "[eval]" =
  List.iter
    [ "13"
    ; "(+ 1 4)"
    ; "((lambda (x) (+ x 1)) 13)"
    ; "(+ \"foo\")"
    ]
    ~f:(fun (string : string) ->
      let value =
        Or_error.try_with (fun () -> eval (string |> read))
      in
      print_s [%message "" ~_:(string : string) "-->" ~_:(value : Value.t Or_error.t)]);
  [%expect {|
    (13 --> (Ok 13))
    ("(+ 1 4)" --> (Ok 5))
    ("((lambda (x) (+ x 1)) 13)" --> (Ok 14))
    ("(+ \"foo\")" --> (Error (wrong-type-argument (number-or-marker-p foo)))) |}];
;;

let%expect_test "[eval_string]" =
  print_s [%sexp (eval_string "(+ 1 2)" : Value.t)];
  [%expect {|
    3 |}];
;;

let symbol_plist symbol =
  Symbol.funcall1 ("symbol-plist" |> Symbol.intern)
    (symbol |> Symbol.to_value)
;;

let%expect_test "[defvar]" =
  let x = "x" |> Symbol.intern in
  defvar [%here] x (13 |> Value.of_int_exn) ~docstring:"some text";
  print_s [%sexp (Current_buffer.value_exn { symbol = x; type_ = Value.Type.int } : int)];
  [%expect {|
    13 |}];
  print_s [%sexp (symbol_plist x : Value.t)];
  [%expect {|
    (custom-group
      ((x-dnd-test-function custom-variable)
       (x-dnd-types-alist   custom-variable)
       (x-dnd-known-types   custom-variable)
       (x-gtk-stock-map     custom-variable)
       (icon-map-list       custom-variable))
      variable-documentation
      "some text") |}];
;;

let%expect_test "[defcustom]" =
  let x = "x" |> Symbol.intern in
  defcustom [%here] x Integer
    ~docstring:"some text"
    ~group:("test" |> Customization.Group.of_string)
    ~standard_value:(13 |> Value.of_int_exn);
  print_s [%sexp (symbol_plist x : Value.t)];
  [%expect {|
    (custom-group
      ((x-dnd-test-function custom-variable)
       (x-dnd-types-alist   custom-variable)
       (x-dnd-known-types   custom-variable)
       (x-gtk-stock-map     custom-variable)
       (icon-map-list       custom-variable))
      variable-documentation
      "some text"
      standard-value
      ((quote 13))
      custom-type
      integer
      custom-requests
      nil) |}];
;;
