open! Core_kernel
open! Import
open! Advice

let%expect_test "" =
  let for_function = "test-function" |> Symbol.intern in
  defun [%here] Value.Type.int for_function Defun.Let_syntax.(
    let%map_open args = rest ("rest" |> Symbol.intern) Value.Type.value in
    print_s [%message "test-function" (args : Value.t list)];
    13);
  let call_test_function () =
    let result =
      Symbol.funcallN for_function ([ 1; 2; 3 ] |> List.map ~f:Value.of_int_exn) in
    print_s [%message "call" (result : Value.t)] in
  let advice_name = "test-advice" |> Symbol.intern in
  Advice.add [%here] Value.Type.int advice_name ~for_function
    (fun ~inner ~inner_args:rest ->
       print_s [%message "advice" (rest : Value.t list)];
       let inner_result = inner ((0 |> Value.of_int_exn) :: rest) in
       print_s [%message "advice" (inner_result : Value.t)];
       (1 + (inner_result |> Value.to_int_exn)));
  call_test_function ();
  [%expect {|
    (advice (rest (1 2 3)))
    (test-function (args (0 1 2 3)))
    (advice (inner_result 13))
    (call (result 14)) |}];
  Advice.remove advice_name ~for_function;
  call_test_function ();
  [%expect {|
    (test-function (args (1 2 3)))
    (call (result 13)) |}];
  Advice.add_predefined_function advice_name ~for_function;
  call_test_function ();
  [%expect {|
    (advice (rest (1 2 3)))
    (test-function (args (0 1 2 3)))
    (advice (inner_result 13))
    (call (result 14)) |}];
;;
