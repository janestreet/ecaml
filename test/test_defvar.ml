open! Core_kernel
open! Import
open! Defvar

let%expect_test "[defvar]" =
  let x = "x" |> Symbol.intern in
  defvar x [%here] ~docstring:"some text" ~initial_value:(13 |> Value.of_int_exn) ();
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
      "some text") |}]
;;

let%expect_test "[defvaralias]" =
  let x = "x" |> Symbol.intern in
  print_endline (Describe.variable x);
  [%expect {|
    x's value is 13

    Documentation:
    some text |}];
  let y1 = "y1" |> Symbol.intern in
  defvaralias y1 [%here] ~alias_of:x ();
  print_endline (Describe.variable y1);
  [%expect
    {|
    y1's value is 13

      This variable is an alias for `x'.

    Documentation:
    some text |}];
  let y2 = "y2" |> Symbol.intern in
  defvaralias y2 [%here] ~alias_of:x ~docstring:"some other text" ();
  print_endline (Describe.variable y2);
  [%expect
    {|
    y2's value is 13

      This variable is an alias for `x'.

    Documentation:
    some other text |}]
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
  print_endline (Describe.variable x);
  [%expect
    {|
    x's value is 13

      This variable is an alias for `y'.
      This variable is obsolete since then;
      use `y' instead.

    Documentation:
    some docs |}]
;;
