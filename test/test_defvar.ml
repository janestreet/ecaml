open! Core_kernel
open! Async_kernel
open! Import
open! Defvar

let%expect_test "[defvar]" =
  let x = "x" |> Symbol.intern in
  ignore
    (defvar x [%here] ~docstring:"some text" ~type_:Value.Type.int ~initial_value:13 ()
     : _ Var.t);
  print_s [%sexp (Current_buffer.value_exn { symbol = x; type_ = Value.Type.int } : int)];
  [%expect {|
    13 |}];
  print_s [%sexp (Plist.of_symbol x : Plist.t)];
  [%expect
    {|
    (custom-group
      ((x-dnd-test-function custom-variable)
       (x-dnd-types-alist   custom-variable)
       (x-dnd-known-types   custom-variable)
       (x-gtk-stock-map     custom-variable)
       (icon-map-list       custom-variable))
      variable-documentation
      "some text") |}];
  return ()
;;

let%expect_test "[defvar] with invalid value" =
  let var =
    defvar
      ("var-with-invalid-valuer" |> Symbol.intern)
      [%here]
      ~docstring:""
      ~type_:Value.Type.int
      ~initial_value:13
      ()
  in
  Current_buffer.set_value { var with type_ = Value.Type.bool } false;
  show_raise (fun () -> Current_buffer.value_exn var);
  [%expect
    {|
    (raised (
      "invalid value for variable: var-with-invalid-valuer"
      ("unable to convert Elisp value to OCaml value"
       (type_ int)
       (value nil)
       (exn (wrong-type-argument (integerp nil)))))) |}];
  return ()
;;

let%expect_test "[defvaralias]" =
  let x = "x" |> Symbol.intern in
  print_endline (Help.describe_variable_text x);
  [%expect {|
    x's value is 13

    Documentation:
    some text |}];
  let y1 = "y1" |> Symbol.intern in
  defvaralias y1 [%here] ~alias_of:x ();
  print_endline (Help.describe_variable_text y1);
  [%expect
    {|
    y1's value is 13

      This variable is an alias for `x'.

    Documentation:
    some text |}];
  let y2 = "y2" |> Symbol.intern in
  defvaralias y2 [%here] ~alias_of:x ~docstring:"some other text" ();
  print_endline (Help.describe_variable_text y2);
  [%expect
    {|
    y2's value is 13

      This variable is an alias for `x'.

    Documentation:
    some other text |}];
  return ()
;;

let%expect_test "[define_obsolete_alias]" =
  let x = "x" |> Symbol.intern in
  define_obsolete_alias
    x
    [%here]
    ~docstring:"some docs"
    ~alias_of:("y" |> Symbol.intern)
    ~since:"then"
    ();
  print_endline (Help.describe_variable_text x);
  [%expect
    {|
    x's value is 13

      This variable is an alias for `y'.
      This variable is obsolete since then;
      use `y' instead.

    Documentation:
    some docs |}];
  return ()
;;
