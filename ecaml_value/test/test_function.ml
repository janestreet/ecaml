open! Core
open! Async_kernel
open! Import
open! Function

let () = Dynamic.set_root Backtrace.elide true

let%expect_test "mutual recursion between Emacs and OCaml" =
  let r = ref None in
  let loop i =
    if i = 0
    then 0
    else (
      match !r with
      | None -> assert false
      | Some v ->
        print_s [%message (i : int)];
        1 + Value.to_int_exn (Value.funcall1 v (i - 1 |> Value.of_int_exn)))
  in
  r
  := Some
       (Function.to_value
          (lambda
             [%here]
             (Returns Value.Type.int)
             (let%map_open.Defun () = return ()
              and i = required "int" int in
              loop i)));
  print_s [%message "result" ~_:(loop 5 : int)];
  [%expect
    {|
    (i 5)
    (i 4)
    (i 3)
    (i 2)
    (i 1)
    (result 5)
    |}];
  return ()
;;

let eval_string s = s |> Form.Blocking.eval_string

let emacs_raise () =
  Funcall.Wrap.("signal" <: Symbol.t @-> int @-> return nil) ("error" |> Symbol.intern) 13
;;

let emacs_try_with =
  "(lambda (f) (condition-case error (funcall f) (error `(emacs-caught-it ,error))))"
  |> eval_string
;;

let ocaml_raise =
  lambda_nullary_nil [%here] (fun () -> raise_s [%message "raising"]) |> Function.to_value
;;

let%expect_test "raising from Emacs to OCaml" =
  show_raise emacs_raise;
  [%expect {| (raised 13) |}];
  return ()
;;

let%expect_test "raising from OCaml to Emacs" =
  print_s [%sexp (Value.funcall1 emacs_try_with ocaml_raise : Value.t)];
  [%expect {| (emacs-caught-it (error raising)) |}];
  return ()
;;

let%expect_test "raising from Emacs to Emacs with OCaml in between" =
  let value =
    Value.funcall1
      emacs_try_with
      (lambda_nullary_nil [%here] (fun () -> emacs_raise ()) |> Function.to_value)
  in
  print_s [%sexp (value : Value.t)];
  [%expect {| (emacs-caught-it (error . 13)) |}];
  return ()
;;

let%expect_test "error-symbol preservation when Emacs signal crosses OCaml" =
  let value =
    Value.funcall1
      ("(lambda (f) (condition-case error (funcall f) (arith-error `(emacs-caught-it \
        ,error))))"
       |> eval_string)
      (lambda_nullary_nil [%here] (fun () ->
         Funcall.Wrap.("signal" <: Symbol.t @-> value @-> return nil)
           ("arith-error" |> Symbol.intern)
           Value.nil)
       |> Function.to_value)
  in
  print_s [%sexp (value : Value.t)];
  [%expect {| (emacs-caught-it (arith-error)) |}];
  return ()
;;

let%expect_test "raising from OCaml to OCaml with Emacs in between" =
  show_raise (fun () ->
    Value.funcall1 ("(lambda (f) (funcall f))" |> eval_string) ocaml_raise);
  [%expect {| (raised (raising)) |}];
  return ()
;;

let%expect_test "raising from OCaml to OCaml through many layers of Emacs" =
  let rec loop n =
    Test_function_file1.funcall0 (fun () ->
      if n <= 0 then raise_s [%message "foo" "bar" "baz"] else loop (n - 1))
  in
  let files_in_backtrace f =
    Backtrace.Exn.with_recording true ~f:(fun () ->
      match f () with
      | () -> []
      | exception _ ->
        let backtrace = Stdlib.Printexc.get_raw_backtrace () in
        (match Stdlib.Printexc.backtrace_slots backtrace with
         | None -> []
         | Some slots ->
           Array.filter_map slots ~f:Stdlib.Printexc.Slot.location
           |> Array.map ~f:(fun slot -> slot.filename)
           |> Array.to_list))
  in
  let files = files_in_backtrace (fun () -> loop 1) in
  (match List.mem files Test_function_file1.filename ~equal:String.( = ) with
   | true -> ()
   | false ->
     print_cr [%message "Backtrace has no frames from" Test_function_file1.filename]);
  [%expect {| |}];
  Current_buffer.set_value_temporarily
    Sync
    (Debugger.debug_on_error |> Customization.var)
    true
    ~f:(fun () ->
      let show_errors = false in
      match show_errors with
      | true ->
        Dynamic.with_temporarily Backtrace.elide false ~f:(fun () ->
          require_does_raise ~show_backtrace:true (fun () -> loop 1))
      | false -> require_does_raise (fun () -> loop 1));
  [%expect
    {|
    (((foo bar baz) (backtrace ("<backtrace elided in test>")))
     (backtrace "<backtrace elided in test>"))
    |}];
  return ()
;;

let%expect_test "function descriptions" =
  let describe_function symbol =
    Form.Blocking.eval_string
      (let symbol = symbol |> Symbol.name in
       [%string
         {|
(with-temp-buffer
  (let ((standard-output (current-buffer)))
    (describe-function-1 '%{symbol})
    (buffer-string)))
|}])
    |> Value.to_utf8_bytes_exn
  in
  let do_nothing _ = Value.nil in
  let function_name = "test-function" |> Symbol.intern in
  let w = "w" |> Symbol.intern in
  let x = "x" |> Symbol.intern in
  let y = "y" |> Symbol.intern in
  let z = "z" |> Symbol.intern in
  let describe ?docstring ?optional_args ?rest_arg () ~args =
    Defun.defalias
      function_name
      ~alias_of:
        (Function.create [%here] do_nothing ?docstring ?optional_args ?rest_arg ~args
         |> Function.to_value)
      ();
    print_endline (describe_function function_name |> hide_positions_in_string)
  in
  describe () ~args:[];
  [%expect
    {|
    a interpreted-function.

    (test-function)

    Implemented at [app/emacs/lib/ecaml_value/test/test_function.ml:LINE:COL].
    |}];
  describe () ~args:[ x ];
  [%expect
    {|
    a interpreted-function.

    (test-function X)

    Implemented at [app/emacs/lib/ecaml_value/test/test_function.ml:LINE:COL].
    |}];
  describe () ~args:[ x; y ];
  [%expect
    {|
    a interpreted-function.

    (test-function X Y)

    Implemented at [app/emacs/lib/ecaml_value/test/test_function.ml:LINE:COL].
    |}];
  describe () ~args:[] ~rest_arg:x;
  [%expect
    {|
    a interpreted-function.

    (test-function &rest X)

    Implemented at [app/emacs/lib/ecaml_value/test/test_function.ml:LINE:COL].
    |}];
  describe () ~args:[ x ] ~rest_arg:y;
  [%expect
    {|
    a interpreted-function.

    (test-function X &rest Y)

    Implemented at [app/emacs/lib/ecaml_value/test/test_function.ml:LINE:COL].
    |}];
  describe () ~args:[] ~optional_args:[ x ];
  [%expect
    {|
    a interpreted-function.

    (test-function &optional X)

    Implemented at [app/emacs/lib/ecaml_value/test/test_function.ml:LINE:COL].
    |}];
  describe () ~args:[] ~optional_args:[ x; y ];
  [%expect
    {|
    a interpreted-function.

    (test-function &optional X Y)

    Implemented at [app/emacs/lib/ecaml_value/test/test_function.ml:LINE:COL].
    |}];
  describe () ~docstring:"custom doc" ~args:[ w ] ~optional_args:[ x; y ] ~rest_arg:z;
  [%expect
    {|
    a interpreted-function.

    (test-function W &optional X Y &rest Z)

    Custom doc

    Implemented at [app/emacs/lib/ecaml_value/test/test_function.ml:LINE:COL].
    |}];
  return ()
;;

let%expect_test "function arguments" =
  let v0 = 0 |> Value.of_int_exn in
  let v1 = 1 |> Value.of_int_exn in
  let v2 = 2 |> Value.of_int_exn in
  let a0 = [] in
  let a1 = [ v0 ] in
  let a2 = [ v0; v1 ] in
  let a3 = [ v0; v1; v2 ] in
  let _w = "w" |> Symbol.intern in
  let x = "x" |> Symbol.intern in
  let y = "y" |> Symbol.intern in
  let z = "z" |> Symbol.intern in
  let arguments ?optional_args ?rest_arg ~args applied_to =
    match
      Value.funcallN_i
        (Function.create [%here] ?optional_args ?rest_arg ~args (fun arguments ->
           print_s [%message (arguments : Value.t array)];
           Value.nil)
         |> Function.to_value)
        applied_to
    with
    | _ -> ()
    | exception _ -> print_s [%message "mismatch"]
  in
  arguments a0 ~args:[];
  [%expect {| (arguments ()) |}];
  arguments a1 ~args:[];
  [%expect {| mismatch |}];
  arguments a0 ~args:[ x ];
  [%expect {| mismatch |}];
  arguments a1 ~args:[ x ];
  [%expect {| (arguments (0)) |}];
  arguments a2 ~args:[ x; y ];
  [%expect {| (arguments (0 1)) |}];
  arguments a0 ~args:[] ~rest_arg:x;
  [%expect {| (arguments ()) |}];
  arguments a1 ~args:[] ~rest_arg:x;
  [%expect {| (arguments (0)) |}];
  arguments a2 ~args:[] ~rest_arg:x;
  [%expect {| (arguments (0 1)) |}];
  arguments a0 ~args:[] ~optional_args:[ x; y ] ~rest_arg:z;
  [%expect {| (arguments (nil nil)) |}];
  arguments a1 ~args:[] ~optional_args:[ x; y ] ~rest_arg:z;
  [%expect {| (arguments (0 nil)) |}];
  arguments a2 ~args:[] ~optional_args:[ x; y ] ~rest_arg:z;
  [%expect {| (arguments (0 1)) |}];
  arguments a3 ~args:[] ~optional_args:[ x; y ] ~rest_arg:z;
  [%expect {| (arguments (0 1 2)) |}];
  return ()
;;

let%expect_test "raise in dispatch" =
  let f = create [%here] ~args:[] (fun _ -> Value.nil) in
  show_raise (fun () ->
    Ref.set_temporarily Expert.raise_in_dispatch true ~f:(fun () ->
      Value.funcall0_i (f |> Function.to_value)));
  [%expect
    {|
    ("Ecaml callback handling raised"
     (exn "Function.Expert.raise_in_dispatch set"))
    (raised (uncaught-ocaml-exception Fdispatch))
    |}];
  return ()
;;

let%expect_test "reporting [Out_of_memory] doesn't allocate" =
  require_no_allocation (fun () ->
    Ecaml_value.Ecaml_callback.report_exn_when_calling_callback Out_of_memory);
  [%expect {| Ecaml received Out_of_memory |}];
  return ()
;;
