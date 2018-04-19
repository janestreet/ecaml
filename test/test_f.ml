open! Core_kernel
open! Import
open! Funcall
open Value.Type

module Q = struct
  let message = "message" |> Symbol.intern
  let point   = "point"   |> Symbol.intern
end

let%expect_test _ =
  let message = Q.message <: string @-> int @-> string @-> string @-> int @-> return nil in
  message "%S %S %S %S" 1 "two" "three" 4;
  [%expect {| 1 "two" "three" 4 |}]
;;

let%expect_test "[nullary]" =
  let point = Q.point <: nullary @-> return int in
  print_s [%sexp (point () : int)];
  [%expect {| 1 |}]
;;

let%expect_test "wrong return type" =
  let locate_file =
    ("locate-file" |> Symbol.intern) <: string @-> list string @-> return string
  in
  print_s [%sexp (Or_error.try_with (fun () -> (locate_file "foo" [])) : string Or_error.t)];
  [%expect {|
    (Error (
      "funcall failed to convert return value."
      (symbol     locate-file)
      (type_.name string)
      (exn (wrong-type-argument (stringp nil))))) |}]
;;
