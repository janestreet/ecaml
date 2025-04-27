open! Core
open! Async_kernel
open! Import
open! Obsolete

let report symbol () = print_s [%message "Called" (symbol : Symbol.t)]

let alias_of =
  let symbol = "current-function" |> Symbol.intern in
  defun_nullary_nil symbol [%here] ~docstring:"<docstring>" (report symbol);
  symbol
;;

let%expect_test "obsolete functions already defined" =
  let obsolete = "foobar-2" |> Symbol.intern in
  defun_nullary_nil obsolete [%here] ~docstring:"_" ~interactive:No_arg (report obsolete);
  print_endline (Help.describe_function_text obsolete);
  [%expect
    {|
    foobar-2 is an interactive interpreted-function.

    (foobar-2)

    _
    |}];
  Defun.define_obsolete_alias obsolete ~alias_of ~since:"now" ();
  print_endline (Help.describe_function_text obsolete);
  [%expect
    {|
    foobar-2 is an alias for `current-function'.

    This function is obsolete since now; use `current-function' instead.

    (foobar-2)

    <docstring>
    |}];
  Symbol.funcall0_i obsolete;
  [%expect {| (Called (symbol current-function)) |}];
  return ()
;;

let%expect_test "documentation for obsolete functions" =
  let obsolete = "foobar-2" |> Symbol.intern in
  Defun.define_obsolete_alias obsolete ~alias_of ~since:"version X.Y" ();
  print_endline (Help.describe_function_text obsolete);
  [%expect
    {|
    foobar-2 is an alias for `current-function'.

    This function is obsolete since version X.Y; use `current-function'
    instead.

    (foobar-2)

    <docstring>
    |}];
  Defun.define_obsolete_alias
    obsolete
    ~docstring:"arbitrary docstring"
    ~alias_of
    ~since:"version X.Y"
    ();
  print_endline (Help.describe_function_text obsolete);
  [%expect
    {|
    foobar-2 is an alias for `current-function'.

    This function is obsolete since version X.Y; use `current-function'
    instead.

    (foobar-2)

    arbitrary docstring
    |}];
  Defun.define_obsolete_alias
    obsolete
    ~docstring:"arbitrary docstring"
    ~alias_of
    ~since:"now"
    ();
  print_endline (Help.describe_function_text obsolete);
  [%expect
    {|
    foobar-2 is an alias for `current-function'.

    This function is obsolete since now; use `current-function' instead.

    (foobar-2)

    arbitrary docstring
    |}];
  return ()
;;

let%expect_test "obsolete functions not yet defined" =
  let obsolete = "foobar" |> Symbol.intern in
  Defun.define_obsolete_alias obsolete ~alias_of ~since:"now" ();
  print_endline (Help.describe_function_text obsolete);
  [%expect
    {|
    foobar is an alias for `current-function'.

    This function is obsolete since now; use `current-function' instead.

    (foobar)

    <docstring>
    |}];
  (* Later definitions override our obsolete. *)
  defun_nullary_nil obsolete [%here] ~docstring:"_" ~interactive:No_arg (report obsolete);
  print_endline (Help.describe_function_text obsolete);
  [%expect
    {|
    foobar is an interactive interpreted-function.

    This function is obsolete since now; use `current-function' instead.

    (foobar)

    _
    |}];
  Symbol.funcall0_i obsolete;
  [%expect {| (Called (symbol foobar)) |}];
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
