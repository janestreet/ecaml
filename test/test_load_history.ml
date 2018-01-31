open! Core_kernel
open! Import
open! Load_history

let%expect_test "\
[defcustom], [defvar], [defun], [update_emacs_with_entries], [defining_file]" =
  let custom = "custom" |> Symbol.intern in
  let var  = "var" |> Symbol.intern in
  let fun_ = "fun" |> Symbol.intern in
  defcustom [%here] custom Boolean
    ~docstring:"custom docstring"
    ~group:("custom-group" |> Customization.Group.of_string)
    ~standard_value:Value.nil;
  defvar [%here] var Value.nil ~docstring:"foo";
  defun_nullary_nil [%here] fun_ Fn.id;
  update_emacs_with_entries ~chop_prefix:"app/emacs/" ~in_dir:"<dir>";
  let show_defining_file symbol =
    print_s [%sexp (defining_file symbol : string option)] in
  show_defining_file custom;
  [%expect {|
    (<dir>/lib/ecaml/test/test_load_history.ml) |}];
  show_defining_file var;
  [%expect {|
    (<dir>/lib/ecaml/test/test_load_history.ml) |}];
  show_defining_file fun_;
  [%expect {|
    (<dir>/lib/ecaml/test/test_load_history.ml) |}];
;;
