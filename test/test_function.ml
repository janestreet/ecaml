open! Core
open! Async
open! Import

let%expect_test "mutual recursion between Emacs and OCaml" =
  let r = ref None in
  let loop i =
    if i = 0
    then 0 |> Value.of_int
    else (
      match !r with
      | None -> assert false
      | Some v ->
        print_s [%message (i : int)];
        Value.of_int (1 + Value.to_int (Value.funcall v [ i - 1 |> Value.of_int ])))
  in
  r := Some (Function.to_value
               (Function.create ~args:[ "int" |> Symbol.intern ]
                  (function
                    | [| v |] -> loop (v |> Value.to_int)
                    | _ -> assert false)));
  print_s [%message "result" ~_:(loop 5 : Value.t)];
  [%expect {|
    (i 5)
    (i 4)
    (i 3)
    (i 2)
    (i 1)
    (result 5) |}];
;;

let eval_string s = s |> Expression.of_string |> Expression.eval

let emacs_raise () =
  Symbol.funcall_i ("signal" |> Symbol.intern)
    [ "error-symbol" |> Symbol.intern |> Symbol.to_value
    ; 13 |> Value.of_int ]
;;

let emacs_try_with =
  "(lambda (f) (condition-case error (funcall f) (error `(emacs-caught-it ,error))))"
  |> eval_string
;;

let ocaml_raise =
  (fun _ -> raise_s [%message "raising"]; Value.nil)
  |> Function.create ~args:[]
  |> Function.to_value
;;

let%expect_test "raising from Emacs to OCaml" =
  show_raise emacs_raise;
  [%expect {|
    (raised (
      signal
      (symbol error-symbol)
      (data   13))) |}];
;;

let%expect_test "raising from OCaml to Emacs" =
  print_s [%sexp (Value.funcall emacs_try_with [ ocaml_raise ] : Value.t)];
  [%expect {|
    (emacs-caught-it (error raising)) |}];
;;

let%expect_test "raising from Emacs to Emacs with OCaml in between" =
  let value =
    Value.funcall
      emacs_try_with
      [ (fun _ -> emacs_raise (); Value.nil)
        |> Function.create ~args:[]
        |> Function.to_value ]
  in
  print_s [%sexp (value : Value.t)];
  [%expect {|
    (emacs-caught-it (error "(signal (symbol error-symbol) (data 13))")) |}];
;;

let%expect_test "raising from OCaml to OCaml with Emacs in between" =
  show_raise (fun () ->
    Value.funcall
      ("(lambda (f) (funcall f))" |> eval_string)
      [ ocaml_raise ]);
  [%expect {|
    (raised (signal (symbol error) (data (raising)))) |}];
;;

let%expect_test "function descriptions" =
  let describe_function =
    fun symbol ->
      Expression.eval
        (Expression.of_string
           (String.concat [ "\
(with-temp-buffer
  (let ((standard-output (current-buffer)))
    (describe-function-1 '"; symbol |> Symbol.name ;")
    (buffer-string)))" ]))
      |> Value.to_string
  in
  let do_nothing _ = Value.nil in
  let function_name = "test-function" |> Symbol.intern in
  let w = "w" |> Symbol.intern in
  let x = "x" |> Symbol.intern in
  let y = "y" |> Symbol.intern in
  let z = "z" |> Symbol.intern in
  let describe ?docstring ?optional_args ?rest_arg () ~args =
    Symbol.fset function_name
      (Function.create do_nothing ?docstring ?optional_args ?rest_arg ~args
       |> Function.to_value);
    print_endline (describe_function function_name);
  in
  describe () ~args:[];
  [%expect {|
    a Lisp function.

    (test-function)

    Not documented. |}] >>= fun () ->
  describe () ~args:[ x ];
  [%expect {|
    a Lisp function.

    (test-function X)

    Not documented. |}] >>= fun () ->
  describe () ~args:[ x; y ];
  [%expect {|
    a Lisp function.

    (test-function X Y)

    Not documented. |}] >>= fun () ->
  describe () ~args:[] ~rest_arg:x;
  [%expect {|
    a Lisp function.

    (test-function &rest X)

    Not documented. |}] >>= fun () ->
  describe () ~args:[ x ] ~rest_arg:y;
  [%expect {|
    a Lisp function.

    (test-function X &rest Y)

    Not documented. |}] >>= fun () ->
  describe () ~args:[ ] ~optional_args:[ x ];
  [%expect {|
    a Lisp function.

    (test-function &optional X)

    Not documented. |}] >>= fun () ->
  describe () ~args:[ ] ~optional_args:[ x; y ];
  [%expect {|
    a Lisp function.

    (test-function &optional X Y)

    Not documented. |}] >>= fun () ->
  describe () ~docstring:"custom doc" ~args:[ w ] ~optional_args:[ x; y ] ~rest_arg:z;
  [%expect {|
    a Lisp function.

    (test-function W &optional X Y &rest Z)

    custom doc |}]
;;

let%expect_test "function arguments" =
  let v0 = 0 |> Value.of_int in
  let v1 = 1 |> Value.of_int in
  let v2 = 2 |> Value.of_int in
  let a0 = [ ] in
  let a1 = [ v0 ] in
  let a2 = [ v0; v1 ] in
  let a3 = [ v0; v1; v2 ] in
  let _w = "w" |> Symbol.intern in
  let x = "x" |> Symbol.intern in
  let y = "y" |> Symbol.intern in
  let z = "z" |> Symbol.intern in
  let arguments ?optional_args ?rest_arg ~args applied_to =
    match
      Value.funcall_i
        (Function.create ?optional_args ?rest_arg ~args
           (fun arguments ->
              print_s [%message (arguments : Value.t array)];
              Value.nil)
         |> Function.to_value)
        applied_to;
    with
    | _ -> ()
    | exception _ -> print_s [%message "mismatch"]
  in
  arguments a0 ~args:[];
  [%expect {|
    (arguments ()) |}] >>= fun () ->
  arguments a1 ~args:[];
  [%expect {|
    mismatch |}] >>= fun () ->
  arguments a0 ~args:[ x ];
  [%expect {|
    mismatch |}] >>= fun () ->
  arguments a1 ~args:[ x ];
  [%expect {|
    (arguments (0)) |}] >>= fun () ->
  arguments a2 ~args:[ x; y ];
  [%expect {|
    (arguments (0 1)) |}] >>= fun () ->
  arguments a0 ~args:[ ] ~rest_arg:x;
  [%expect {|
    (arguments ()) |}] >>= fun () ->
  arguments a1 ~args:[ ] ~rest_arg:x;
  [%expect {|
    (arguments (0)) |}] >>= fun () ->
  arguments a2 ~args:[ ] ~rest_arg:x;
  [%expect {|
    (arguments (0 1)) |}] >>= fun () ->
  arguments a0 ~args:[ ] ~optional_args:[ x; y ] ~rest_arg:z;
  [%expect {|
    (arguments (nil nil)) |}] >>= fun () ->
  arguments a1 ~args:[ ] ~optional_args:[ x; y ] ~rest_arg:z;
  [%expect {|
    (arguments (0 nil)) |}] >>= fun () ->
  arguments a2 ~args:[ ] ~optional_args:[ x; y ] ~rest_arg:z;
  [%expect {|
    (arguments (0 1)) |}] >>= fun () ->
  arguments a3 ~args:[ ] ~optional_args:[ x; y ] ~rest_arg:z;
  [%expect {|
    (arguments (0 1 2)) |}] >>= fun () ->
  return ();
;;
