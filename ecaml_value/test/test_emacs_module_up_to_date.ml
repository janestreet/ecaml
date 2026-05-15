open! Core
open! Async_kernel
open! Import

let invocation_directory = Var.Wrap.("invocation-directory" <: string)

let%expect_test "[emacs-module.h] is up to date" =
  (* emacs-module.h is installed in $prefix/include/ alongside the binary in $prefix/bin/.
     invocation-directory is $prefix/bin/. *)
  let invocation_dir = Current_buffer.value_exn invocation_directory in
  let prefix = Filename.directory_exn (Filename.of_directory invocation_dir) in
  let expected_file = prefix ^/ "include" ^/ "emacs-module.h" in
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
