open! Core
open! Async_kernel
open! Import

let c_source_directory = Var.Wrap.("find-function-C-source-directory" <: string)

let%expect_test "[emacs-module.h] is up to date" =
  let expected_file = Current_buffer.value_exn c_source_directory ^/ "emacs-module.h" in
  let expected_contents = In_channel.read_all expected_file in
  let actual_contents = In_channel.read_all "../src/emacs-module.h" in
  require
    (String.equal expected_contents actual_contents)
    ~if_false_then_print_s:
      [%lazy_sexp
        "app/emacs/lib/ecaml_value/src/emacs-module.h is out of date."
        , { should_be_identical_to = (expected_file : string) }];
  [%expect {| |}];
  return ()
;;
