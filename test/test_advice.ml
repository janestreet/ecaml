open! Core_kernel
open! Import
open! Advice

let%expect_test "" =
  let test_function = "test-function" |> Symbol.intern in
  defun [%here] ~args:[] ~rest_arg:Q.rest test_function
    (fun args ->
       print_s [%message "test-function" (args : Value.t array)];
       (13 |> Value.of_int_exn));
  let call_test_function () =
    let result =
      Symbol.funcallN test_function ([ 1; 2; 3 ] |> List.map ~f:Value.of_int_exn) in
    print_s [%message "call" (result : Value.t)] in
  let advice_name = Advice.Name.of_symbol ("test-advice" |> Symbol.intern) in
  let defadvice () =
    defadvice [%here]
      ~advice_name
      ~for_function:test_function
      (fun ~args ~inner ->
         print_s [%message "advice" (args : Value.t list)];
         let inner_result = inner ((0 |> Value.of_int_exn) :: args) in
         print_s [%message "advice" (inner_result : Value.t)];
         (1 + (inner_result |> Value.to_int_exn)) |> Value.of_int_exn) in
  defadvice ();
  call_test_function ();
  [%expect {|
    (test-function (args (1 2 3)))
    (call (result 13)) |}];
  activate_function test_function;
  call_test_function ();
  [%expect {|
    (advice (args (1 2 3)))
    (test-function (args (0 1 2 3)))
    (advice (inner_result 13))
    (call (result 14)) |}];
  disable advice_name test_function;
  activate_function test_function;
  call_test_function ();
  [%expect {|
    (test-function (args (1 2 3)))
    (call (result 13)) |}];
  enable advice_name test_function;
  activate_function test_function;
  call_test_function ();
  [%expect {|
    (advice (args (1 2 3)))
    (test-function (args (0 1 2 3)))
    (advice (inner_result 13))
    (call (result 14)) |}];
  deactivate_function test_function;
  call_test_function ();
  [%expect {|
    (test-function (args (1 2 3)))
    (call (result 13)) |}];
  activate_all ();
  call_test_function ();
  [%expect {|
    (advice (args (1 2 3)))
    (test-function (args (0 1 2 3)))
    (advice (inner_result 13))
    (call (result 14)) |}];
  deactivate_all ();
  call_test_function ();
  [%expect {|
    (test-function (args (1 2 3)))
    (call (result 13)) |}];
  activate_all ();
  call_test_function ();
  [%expect {|
    (advice (args (1 2 3)))
    (test-function (args (0 1 2 3)))
    (advice (inner_result 13))
    (call (result 14)) |}];
  unadvise_function test_function;
  call_test_function ();
  [%expect {|
    (test-function (args (1 2 3)))
    (call (result 13)) |}];
  defadvice ();
  activate_all ();
  call_test_function ();
  [%expect {|
    (advice (args (1 2 3)))
    (test-function (args (0 1 2 3)))
    (advice (inner_result 13))
    (call (result 14)) |}];
  unadvise_all ();
  call_test_function ();
  [%expect {|
    (test-function (args (1 2 3)))
    (call (result 13)) |}];
  defadvice ();
  activate_functions_with_advice_matching ("est-ad" |> Regexp.quote);
  call_test_function ();
  [%expect {|
    (advice (args (1 2 3)))
    (test-function (args (0 1 2 3)))
    (advice (inner_result 13))
    (call (result 14)) |}];
  deactivate_functions_with_advice_matching ("est-ad" |> Regexp.quote);
  call_test_function ();
  [%expect {|
    (test-function (args (1 2 3)))
    (call (result 13)) |}];
;;
