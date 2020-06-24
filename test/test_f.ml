open! Core_kernel
open! Async_kernel
open! Import
open! Funcall.Wrap
open Value.Type

let%expect_test _ =
  let message =
    "message" <: string @-> int @-> string @-> string @-> int @-> return nil
  in
  message "%S %S %S %S" 1 "two" "three" 4;
  [%expect {| 1 "two" "three" 4 |}];
  Deferred.return ()
;;

let%expect_test "[nullary]" =
  let point = "point" <: nullary @-> return int in
  print_s [%sexp (point () : int)];
  [%expect {| 1 |}];
  Deferred.return ()
;;

let%expect_test "wrong return type" =
  let locate_file = "locate-file" <: string @-> list string @-> return string in
  print_s
    [%sexp (Or_error.try_with (fun () -> locate_file "foo" []) : string Or_error.t)];
  [%expect
    {|
    (Error (
      "funcall failed to convert return value."
      (symbol locate-file)
      (type_  string)
      (exn (
        "unable to convert Elisp value to OCaml value"
        (type_ string)
        (value nil)
        (exn (wrong-type-argument (stringp nil))))))) |}];
  Deferred.return ()
;;
