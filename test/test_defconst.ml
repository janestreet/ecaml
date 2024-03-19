open! Core
open! Async_kernel
open! Import
open! Defconst

let%expect_test "defconst" =
  let symbol = "foo" |> Symbol.intern in
  let var =
    defconst symbol [%here] ~docstring:"_" ~type_:Value.Type.string ~value:"foo-value"
  in
  Load_history.update_emacs_with_entries ~chop_prefix:"app/emacs/" ~in_dir:"<dir>";
  print_endline (Help.describe_variable_text symbol);
  [%expect
    {|
    foo's value is "foo-value"

    _

      This variable may be risky if used as a file-local variable.
      Probably introduced at or before Emacs version 1.2.
    |}];
  print_endline (Current_buffer.value_exn var);
  [%expect {| foo-value |}];
  print_s [%sexp (Load_history.defining_file symbol : string option)];
  [%expect {| (<dir>/lib/ecaml/test/test_defconst.ml) |}];
  return ()
;;
