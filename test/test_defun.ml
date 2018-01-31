open! Core_kernel
open! Import
open Defun

let here = [%here]

let return_type = Value.Type.sexpable (module Sexp) ~name:[%sexp "sexp"]

let print_funcallN symbol args =
  print_s (return_type.of_value_exn (Symbol.funcallN symbol args))
;;

let%expect_test "[defun]" =
  let symbol = Symbol.gensym () in
  defun here return_type symbol
    ~docstring:"Returns its own arguments as a sexp."
    Let_syntax.(
      let%map_open () = return ()
      and i    = required ("int"             |> Symbol.intern) Value.Type.int
      and s    = required ("string"          |> Symbol.intern) Value.Type.string
      and s_o  = optional ("string-optional" |> Symbol.intern) Value.Type.string
      and rest = rest     ("rest"            |> Symbol.intern) Value.Type.string
      in
      [%message
        "Got args."
          (i : int)
          (s : string)
          (s_o : string option)
          (rest : string list)]);
  print_funcallN symbol
    [ 1 |> Value.Type.int.to_value
    ; "two" |> Value.Type.string.to_value
    ; "three" |> Value.Type.string.to_value
    ; "four" |> Value.Type.string.to_value
    ; "five" |> Value.Type.string.to_value
    ];
  [%expect {|
    ("Got args."
      (i 1)
      (s two)
      (s_o (three))
      (rest (four five))) |}];
  print_endline (describe_function ~obscure_symbol:true symbol);
  [%expect {|
<SYMBOL> is a Lisp function.

(<SYMBOL> INT STRING &optional STRING-OPTIONAL &rest REST)

Returns its own arguments as a sexp. |}]
;;

let%expect_test "[defun] tuple ordering" =
  let symbol = Symbol.gensym () in
  defun here return_type symbol
    ~docstring:""
    Let_syntax.(
      let%map_open () = return ()
      and difference =
        let%map minuend    = required ("minuend"    |> Symbol.intern) Value.Type.int
        and     subtrahend = required ("subtrahend" |> Symbol.intern) Value.Type.int
        in
        minuend - subtrahend
      in
      [%message (difference : int)]);
  print_funcallN symbol
    [ 2 |> Value.Type.int.to_value
    ; 1 |> Value.Type.int.to_value
    ];
  [%expect {|
    (difference 1) |}];
  print_endline (describe_function ~obscure_symbol:true symbol);
  [%expect {|
<SYMBOL> is a Lisp function.

(<SYMBOL> MINUEND SUBTRAHEND) |}]
;;

let%expect_test "[defun] wrong number of arguments" =
  let symbol = Symbol.gensym () in
  defun here return_type symbol ~docstring:""
    (let open Defun.Let_syntax in
     let%map_open () = return ()
     and arg = required ("arg" |> Symbol.intern) Value.Type.int
     in
     [%message (arg : int)]);
  print_funcallN symbol (List.init 1 ~f:Value.Type.int.to_value);
  [%expect {| (arg 0) |}];
  show_raise (fun () ->
    Value.For_testing.map_elisp_signal_omit_data (fun () ->
      print_funcallN symbol (List.init 2 ~f:Value.Type.int.to_value)));
  [%expect {|
    (raised wrong-number-of-arguments) |}];
  show_raise (fun () ->
    Value.For_testing.map_elisp_signal_omit_data (fun () ->
      print_funcallN symbol (List.init 0 ~f:Value.Type.int.to_value)));
  [%expect {|
    (raised wrong-number-of-arguments) |}];
;;

let%expect_test "[defun] omitted optional arguments" =
  let symbol = Symbol.gensym () in
  defun here return_type symbol ~docstring:"" Let_syntax.(
    let%map_open () = return ()
    and optional = optional ("optional" |> Symbol.intern) Value.Type.int
    in
    [%message (optional : int option)]);
  print_funcallN symbol (List.init 1 ~f:Value.Type.int.to_value);
  [%expect {| (optional (0)) |}];
  print_funcallN symbol (List.init 0 ~f:Value.Type.int.to_value);
  [%expect {| (optional ()) |}];
;;

let%expect_test "[lambda]" =
  let fn =
    lambda [%here] Value.Type.int
      (let open Defun.Let_syntax in
       let%map_open () = return ()
       and i = required ("int" |> Symbol.intern) Value.Type.int
       in
       i + 1)
  in
  let retval =
    Value.funcall1 (fn |> Function.to_value)
      (1 |> Value.Type.int.to_value)
    |> Value.Type.int.of_value_exn
  in
  print_s [%sexp (retval : int)];
  [%expect {| 2 |}];
  let docstring =
    Symbol.funcall1 ("documentation" |> Symbol.intern)
      (fn |> Function.to_value)
    |> Value.Type.string.of_value_exn
  in
  if not (String.is_prefix docstring ~prefix:"Implemented at")
  then print_endline docstring;
  [%expect {| |}];
;;
