open! Core_kernel
open! Async_kernel
open! Import

(* We don't [open Defun] here because it conflicts with Async's [Let_syntax]. *)

let return = Deferred.return
let here = [%here]
let return_type = Value.Type.sexpable (module Sexp) ~name:[%sexp "sexp"]

let print_funcallN symbol args =
  print_s (Value.Type.of_value_exn return_type (Symbol.funcallN symbol args))
;;

let%expect_test "[defun]" =
  let symbol = Symbol.gensym () in
  defun
    symbol
    here
    ~docstring:"Returns its own arguments as a sexp."
    (Returns return_type)
    (let%map_open.Defun () = return ()
     and i = required "int" int
     and s = required "string" string
     and s_o = optional "string-optional" string
     and rest = rest "rest" string in
     [%message
       "Got args." (i : int) (s : string) (s_o : string option) (rest : string list)]);
  print_funcallN
    symbol
    [ (1 |> Value.Type.(int |> to_value))
    ; ("two" |> Value.Type.(string |> to_value))
    ; ("three" |> Value.Type.(string |> to_value))
    ; ("four" |> Value.Type.(string |> to_value))
    ; ("five" |> Value.Type.(string |> to_value))
    ];
  [%expect
    {|
    ("Got args."
      (i 1)
      (s two)
      (s_o (three))
      (rest (four five))) |}];
  print_endline (Help.describe_function_text ~obscure_symbol:true symbol);
  [%expect
    {|
<SYMBOL> is a Lisp function.

(<SYMBOL> INT STRING &optional STRING-OPTIONAL &rest REST)

Returns its own arguments as a sexp. |}];
  return ()
;;

let%expect_test "[defun ~interactive:(Args _)]" =
  let symbol = "test-interactive" |> Symbol.intern in
  defun
    symbol
    [%here]
    ~docstring:""
    ~interactive:(Args (fun () -> return [ 13 |> Value.of_int_exn ]))
    (Returns Value.Type.unit)
    (let%map_open.Defun () = return ()
     and arg = required "arg" int in
     print_s [%sexp (arg : int)]);
  Symbol.funcall1_i symbol (15 |> Value.of_int_exn);
  [%expect {| 15 |}];
  let%bind () = Command.call_interactively (symbol |> Symbol.to_value) in
  [%expect {| 13 |}];
  return ()
;;

let%expect_test "[defun] tuple ordering" =
  let symbol = Symbol.gensym () in
  defun
    symbol
    here
    ~docstring:""
    (Returns return_type)
    (let%map_open.Defun () = return ()
     and difference =
       let%map_open.Defun minuend = required "minuend" Value.Type.int
       and subtrahend = required "subtrahend" Value.Type.int in
       minuend - subtrahend
     in
     [%message (difference : int)]);
  print_funcallN
    symbol
    [ (2 |> Value.Type.(int |> to_value)); (1 |> Value.Type.(int |> to_value)) ];
  [%expect {|
    (difference 1) |}];
  print_endline (Help.describe_function_text ~obscure_symbol:true symbol);
  [%expect {|
<SYMBOL> is a Lisp function.

(<SYMBOL> MINUEND SUBTRAHEND) |}];
  return ()
;;

let%expect_test "[defun] wrong number of arguments" =
  let symbol = Symbol.gensym () in
  defun
    symbol
    here
    ~docstring:""
    (Returns return_type)
    (let open Defun.Let_syntax in
     let%map_open () = return ()
     and arg = required "arg" int in
     [%message (arg : int)]);
  print_funcallN symbol (List.init 1 ~f:Value.Type.(int |> to_value));
  [%expect {| (arg 0) |}];
  show_raise (fun () ->
    Value.For_testing.map_elisp_signal_omit_data (fun () ->
      print_funcallN symbol (List.init 2 ~f:Value.Type.(int |> to_value))));
  [%expect {| (raised wrong-number-of-arguments) |}];
  show_raise (fun () ->
    Value.For_testing.map_elisp_signal_omit_data (fun () ->
      print_funcallN symbol (List.init 0 ~f:Value.Type.(int |> to_value))));
  [%expect {| (raised wrong-number-of-arguments) |}];
  return ()
;;

let%expect_test "[defun] omitted optional arguments" =
  let symbol = Symbol.gensym () in
  defun
    symbol
    here
    ~docstring:""
    (Returns return_type)
    (let%map_open.Defun () = return ()
     and optional = optional "optional" int in
     [%message (optional : int option)]);
  print_funcallN symbol (List.init 1 ~f:Value.Type.(int |> to_value));
  [%expect {| (optional (0)) |}];
  print_funcallN symbol (List.init 0 ~f:Value.Type.(int |> to_value));
  [%expect {| (optional ()) |}];
  return ()
;;

let%expect_test "[defun] type errors in required, optional, and rest arguments" =
  let test make_arg =
    let symbol = "some-function" |> Symbol.intern in
    defun
      symbol
      [%here]
      ~docstring:""
      (Returns Value.Type.unit)
      (let%map_open.Defun () = return ()
       and _ = make_arg "arg" int in
       assert false);
    require_does_raise [%here] ~hide_positions:true (fun () ->
      Symbol.funcall1_i symbol ("foo" |> Value.of_utf8_bytes))
  in
  test Defun.required;
  [%expect
    {|
    ((
      "function got argument of wrong type"
      some-function
      (defined_at app/emacs/lib/ecaml/test/test_defun.ml:LINE:COL)
      (arg arg)
      ("unable to convert Elisp value to OCaml value"
       (type_ int)
       (value foo)
       (exn (wrong-type-argument (integerp foo)))))) |}];
  test Defun.optional;
  [%expect
    {|
    ((
      "function got argument of wrong type"
      some-function
      (defined_at app/emacs/lib/ecaml/test/test_defun.ml:LINE:COL)
      (arg arg)
      ("unable to convert Elisp value to OCaml value"
       (type_ (nil_or int))
       (value foo)
       (exn (
         "unable to convert Elisp value to OCaml value"
         (type_ int)
         (value foo)
         (exn (wrong-type-argument (integerp foo)))))))) |}];
  test Defun.rest;
  [%expect
    {|
    ((
      "function got argument of wrong type"
      some-function
      (defined_at app/emacs/lib/ecaml/test/test_defun.ml:LINE:COL)
      (arg arg)
      ("unable to convert Elisp value to OCaml value"
       (type_ int)
       (value foo)
       (exn (wrong-type-argument (integerp foo)))))) |}];
  return ()
;;

let%expect_test "[lambda]" =
  let fn =
    lambda
      [%here]
      (Returns Value.Type.int)
      (let open Defun.Let_syntax in
       let%map_open () = return ()
       and i = required "int" int in
       i + 1)
  in
  let retval =
    Value.funcall1 (fn |> Function.to_value) (1 |> Value.Type.(int |> to_value))
    |> Value.Type.(int |> of_value_exn)
  in
  print_s [%sexp (retval : int)];
  [%expect {| 2 |}];
  let docstring = Funcall.Wrap.("documentation" <: Function.t @-> return string) fn in
  if not (String.is_prefix docstring ~prefix:"Implemented at")
  then print_endline docstring;
  [%expect {| |}];
  return ()
;;

let%expect_test "[defalias]" =
  let f = "f" |> Symbol.intern in
  defalias f [%here] ~alias_of:("+" |> Symbol.intern) ();
  print_endline (Help.describe_function_text f);
  [%expect
    {|
    f is an alias for `+'.

    (f &rest NUMBERS-OR-MARKERS)

    Return sum of any number of arguments, which are numbers or markers. |}];
  return ()
;;
