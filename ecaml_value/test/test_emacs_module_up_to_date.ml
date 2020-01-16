open! Core
open! Async_kernel
open! Import

let%expect_test "[emacs-module.h] is up to date" =
  let expected_file =
    Jenga_rules_integration_normal_scrutiny.Emacs.emacs_build_dir ^/ "src/emacs-module.h"
  in
  let expected_contents = In_channel.read_all expected_file in
  let actual_contents = In_channel.read_all "../src/emacs-module.h" in
  require
    [%here]
    (String.equal expected_contents actual_contents)
    ~if_false_then_print_s:
      (lazy
        [%sexp
          "app/emacs/lib/ecaml_value/src/emacs-module.h is out of date."
        , { should_be_identical_to = (expected_file : string) }]);
  [%expect {| |}];
  return ()
;;
