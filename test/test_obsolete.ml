open! Core_kernel
open! Import
open! Obsolete

let report symbol () = print_s [%message "Called" (symbol : Symbol.t)]

let current =
  let symbol = "current-function" |> Symbol.intern in
  defun_nullary_nil [%here] symbol (report symbol);
  symbol
;;

let%expect_test "obsolete functions already defined" =
  let obsolete = "foobar-2" |> Symbol.intern in
  defun_nullary_nil [%here] obsolete (report obsolete) ~docstring:"_" ~interactive:No_arg;
  print_endline (describe_function obsolete);
  [%expect {|
    foobar-2 is an interactive Lisp function.

    (foobar-2)

    _ |}];
  alias obsolete ~current;
  print_endline (describe_function obsolete);
  [%expect
    {|
    foobar-2 is an alias for `current-function'.

    (foobar-2)

    This function is obsolete;
    use `current-function' instead. |}];
  Symbol.funcall0_i obsolete;
  [%expect {| (Called (symbol current-function)) |}]
;;

let%expect_test "documentation for obsolete functions" =
  let obsolete = "foobar-2" |> Symbol.intern in
  alias obsolete ~current ~when_:"version X.Y";
  print_endline (describe_function obsolete);
  [%expect
    {|
    foobar-2 is an alias for `current-function'.

    (foobar-2)

    This function is obsolete since version X.Y;
    use `current-function' instead. |}];
  alias obsolete ~current ~when_:"version X.Y" ~docstring:"arbitrary docstring";
  print_endline (describe_function obsolete);
  [%expect
    {|
    foobar-2 is an alias for `current-function'.

    (foobar-2)

    This function is obsolete since version X.Y;
    use `current-function' instead.

    arbitrary docstring |}];
  alias obsolete ~current ~docstring:"arbitrary docstring";
  print_endline (describe_function obsolete);
  [%expect
    {|
    foobar-2 is an alias for `current-function'.

    (foobar-2)

    This function is obsolete;
    use `current-function' instead.

    arbitrary docstring |}]
;;

let%expect_test "obsolete functions not yet defined" =
  let obsolete = "foobar" |> Symbol.intern in
  alias obsolete ~current;
  print_endline (describe_function obsolete);
  [%expect
    {|
    foobar is an alias for `current-function'.

    (foobar)

    This function is obsolete;
    use `current-function' instead. |}];
  (* Later definitions override our obsolete. *)
  defun_nullary_nil [%here] obsolete (report obsolete) ~docstring:"_" ~interactive:No_arg;
  print_endline (describe_function obsolete);
  [%expect
    {|
    foobar is an interactive Lisp function.

    (foobar)

    This function is obsolete;
    use `current-function' instead.

    _ |}];
  Symbol.funcall0_i obsolete;
  [%expect {| (Called (symbol foobar)) |}]
;;
