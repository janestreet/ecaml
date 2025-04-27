open! Core
open! Async_kernel
open! Import
open! Advice

let test_function = "test-function" |> Symbol.intern

let initialize () =
  defun
    test_function
    [%here]
    ~docstring:"<docstring>"
    (Returns Value.Type.int)
    (let open Defun.Let_syntax in
     let%map_open args = rest "rest" value in
     print_s [%message "test-function" (args : Value.t list)];
     13)
;;

let call_test_function () =
  let result = Symbol.funcall1 test_function (Value.of_int_exn 1) in
  print_s [%message "call" (result : Value.t)]
;;

let advice_name = "test-advice" |> Symbol.intern

let%expect_test "" =
  initialize ();
  let t =
    Advice.defun_around_values
      advice_name
      ~docstring:"<docstring>"
      Sync
      (fun inner rest ->
         print_s [%message "advice" (rest : Value.t list)];
         let inner_result = inner ((0 |> Value.of_int_exn) :: rest) in
         print_s [%message "advice" (inner_result : Value.t)];
         Value.Type.(int |> to_value) (1 + (inner_result |> Value.to_int_exn)))
  in
  Advice.add t ~to_function:test_function;
  call_test_function ();
  [%expect
    {|
    (advice (rest (1)))
    (test-function (args (0 1)))
    (advice (inner_result 13))
    (call (result 14))
    |}];
  Advice.remove t ~from_function:test_function;
  call_test_function ();
  [%expect
    {|
    (test-function (args (1)))
    (call (result 13))
    |}];
  Advice.add (Advice.of_function advice_name) ~to_function:test_function;
  call_test_function ();
  [%expect
    {|
    (advice (rest (1)))
    (test-function (args (0 1)))
    (advice (inner_result 13))
    (call (result 14))
    |}];
  return ()
;;

let%expect_test "[around_funcall ~on_parse_error]" =
  initialize ();
  let test ?on_parse_error arg_type =
    let t =
      Advice.defun_around_funcall
        advice_name
        ~docstring:"<docstring>"
        Funcall.Wrap.(arg_type @-> return int)
        ?on_parse_error
        (fun _ _ ->
          print_s [%message "Advice got called."];
          -1)
    in
    Advice.add t ~to_function:test_function;
    call_test_function ();
    Advice.remove t ~from_function:test_function
  in
  test Value.Type.int;
  [%expect
    {|
    "Advice got called."
    (call (result -1))
    |}];
  show_raise (fun () -> test Value.Type.string);
  [%expect
    {|
    (raised ((
      "Advice failed to parse its arguments"
      app/emacs/lib/ecaml/test/test_advice.ml:72:6
      ("unable to convert Elisp value to OCaml value"
       (type_ string)
       (value 1)
       (exn (wrong-type-argument (stringp 1)))))))
    |}];
  show_raise (fun () -> test Value.Type.string ~on_parse_error:Allow_raise);
  [%expect
    {|
    (raised ((
      "Advice failed to parse its arguments"
      app/emacs/lib/ecaml/test/test_advice.ml:72:6
      ("unable to convert Elisp value to OCaml value"
       (type_ string)
       (value 1)
       (exn (wrong-type-argument (stringp 1)))))))
    |}];
  test Value.Type.string ~on_parse_error:Call_inner_function;
  [%expect
    {|
    (test-function (args (1)))
    (call (result 13))
    |}];
  return ()
;;

let%expect_test "[around_funcall] with arity mismatch" =
  initialize ();
  let advice_body () =
    print_s [%message "Advice got called."];
    -1
  in
  let test ?on_parse_error funcall f =
    let t =
      Advice.defun_around_funcall
        advice_name
        ~docstring:"<docstring>"
        funcall
        ?on_parse_error
        f
    in
    Advice.add t ~to_function:test_function;
    call_test_function ();
    Advice.remove t ~from_function:test_function
  in
  (* correct arity *)
  test Funcall.Wrap.(int @-> return int) (fun _ _ -> advice_body ());
  [%expect
    {|
    "Advice got called."
    (call (result -1))
    |}];
  (* requires more arguments than the underlying *)
  show_raise (fun () ->
    test Funcall.Wrap.(int @-> int @-> return int) (fun _ _ _ -> advice_body ()));
  [%expect
    {|
    (raised ((
      "Advice failed to parse its arguments"
      app/emacs/lib/ecaml/test/test_advice.ml:130:6
      ("unable to convert Elisp value to OCaml value"
       (type_ int)
       (value nil)
       (exn (wrong-type-argument (integerp nil)))))))
    |}];
  (* permits fewer arguments than the underlying *)
  show_raise (fun () ->
    test Funcall.Wrap.(nullary @-> return int) (fun _ () -> advice_body ()));
  [%expect
    {|
    (raised ((
      "Advice failed to parse its arguments"
      app/emacs/lib/ecaml/test/test_advice.ml:130:6
      ("Extra args." ("arity t" 0) (args (1))))))
    |}];
  return ()
;;

let%expect_test "Async advice" =
  initialize ();
  let t =
    Advice.defun_around_values
      advice_name
      ~docstring:"<docstring>"
      Async
      (fun inner rest ->
         let%map () = Clock_ns.after (sec_ns 0.001) in
         print_s [%message "advice" (rest : Value.t list)];
         let inner_result = inner ((0 |> Value.of_int_exn) :: rest) in
         print_s [%message "advice" (inner_result : Value.t)];
         Value.Type.(int |> to_value) (1 + (inner_result |> Value.to_int_exn)))
  in
  Advice.add t ~to_function:test_function;
  let call_test_function () = Async_ecaml.Private.run_outside_async call_test_function in
  let%bind () = call_test_function () in
  [%expect
    {|
    (advice (rest (1)))
    (test-function (args (0 1)))
    (advice (inner_result 13))
    (call (result 14))
    |}];
  Advice.remove t ~from_function:test_function;
  let%bind () = call_test_function () in
  [%expect
    {|
    (test-function (args (1)))
    (call (result 13))
    |}];
  Advice.add (Advice.of_function advice_name) ~to_function:test_function;
  let%bind () = call_test_function () in
  [%expect
    {|
    (advice (rest (1)))
    (test-function (args (0 1)))
    (advice (inner_result 13))
    (call (result 14))
    |}];
  return ()
;;
