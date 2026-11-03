open! Core
open! Async_kernel
open! Import
open! Obsolete

let report symbol () = print_s [%message "Called" (symbol : Symbol.t)]

let%expect_test "[defun ~obsoletes]" =
  let symbol = "brand-new-function" |> Symbol.intern in
  let obsolete = "crusty-old-function" |> Symbol.intern in
  defun_nullary_nil
    symbol
    [%here]
    ~docstring:"<docstring>"
    ~obsoletes:(obsolete, Since "now")
    (report symbol);
  print_endline (Help.describe_function_text obsolete);
  [%expect
    {|
    crusty-old-function is an alias for `brand-new-function'.

    This function is obsolete since now; use `brand-new-function' instead.

    (crusty-old-function)

    <docstring>
    |}];
  Symbol.funcall0_i obsolete;
  [%expect {| (Called (symbol brand-new-function)) |}];
  return ()
;;

let%expect_test "obsolete an undefined variable" =
  let obsolete = "obsolete1" |> Symbol.intern in
  let current = "current1" |> Symbol.intern in
  let show () =
    print_endline (Help.describe_variable_text obsolete);
    print_endline "";
    print_endline (Help.describe_variable_text current)
  in
  show ();
  [%expect
    {|
    obsolete1 is void as a variable.

    Not documented as a variable.

    current1 is void as a variable.

    Not documented as a variable.
    |}];
  make_variable_obsolete obsolete ~current:(Some current) ~since:"now";
  show ();
  [%expect
    {|
    obsolete1 is void as a variable.

    This variable is obsolete since now; use `current1' instead.

    Not documented as a variable.

    current1 is void as a variable.

    Not documented as a variable.
    |}];
  return ()
;;

let%expect_test "obsolete an defined variable" =
  let obsolete = "obsolete2" |> Symbol.intern in
  let current = "current2" |> Symbol.intern in
  ignore
    (defvar
       obsolete
       [%here]
       ~docstring:"an obsolete variable"
       ~type_:Value.Type.bool
       ~initial_value:false
       ()
     : _ Var.t);
  let show () =
    print_endline (Help.describe_variable_text obsolete);
    print_endline "";
    print_endline (Help.describe_variable_text current)
  in
  show ();
  [%expect
    {|
    obsolete2's value is nil

    an obsolete variable

    current2 is void as a variable.

    Not documented as a variable.
    |}];
  make_variable_obsolete obsolete ~current:(Some current) ~since:"now";
  show ();
  [%expect
    {|
    obsolete2's value is nil

    This variable is obsolete since now; use `current2' instead.

    an obsolete variable

    current2 is void as a variable.

    Not documented as a variable.
    |}];
  return ()
;;

let%expect_test "define an obsoleted variable" =
  let obsolete = "obsolete3" |> Symbol.intern in
  let current = "current3" |> Symbol.intern in
  make_variable_obsolete obsolete ~current:(Some current) ~since:"now";
  ignore
    (defvar
       obsolete
       [%here]
       ~docstring:"an obsolete variable"
       ~type_:Value.Type.bool
       ~initial_value:false
       ()
     : _ Var.t);
  print_endline (Help.describe_variable_text obsolete);
  [%expect
    {|
    obsolete3's value is nil

    This variable is obsolete since now; use `current3' instead.

    an obsolete variable
    |}];
  return ()
;;

let%expect_test "define an obsoleted variable with no current replacement" =
  let obsolete = "obsolete4" |> Symbol.intern in
  make_variable_obsolete obsolete ~current:None ~since:"now";
  ignore
    (defvar
       obsolete
       [%here]
       ~docstring:"an obsolete variable"
       ~type_:Value.Type.bool
       ~initial_value:false
       ()
     : _ Var.t);
  print_endline (Help.describe_variable_text obsolete);
  [%expect
    {|
    obsolete4's value is nil

    This variable is obsolete since now.

    an obsolete variable
    |}];
  return ()
;;
