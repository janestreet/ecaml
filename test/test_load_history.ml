open! Core_kernel
open! Import
open! Load_history

let%expect_test "[add_defuns], [defining_file]" =
  let symbol = "zzz" |> Symbol.intern in
  Function.defun [%here] symbol ~args:[] (fun _ -> Value.nil);
  add_defuns ~chop_prefix:"app/emacs/" ~in_dir:"<dir>";
  print_s [%sexp (defining_file symbol : string option)];
  [%expect {|
    (<dir>/lib/ecaml/test/test_load_history.ml) |}];
;;
