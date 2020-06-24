open! Core_kernel
open! Async_kernel
open! Import
open! Value
open! For_testing

let show t = print_s [%sexp (t : t)]
let test_predicate f t = print_s [%sexp (f t : bool)]

let%expect_test "[sexp_of_t] for strings" =
  let test string = show (string |> of_utf8_bytes) in
  test "";
  [%expect {| "" |}];
  test "a";
  [%expect {| a |}];
  test "()";
  [%expect {| () |}];
  test "(a b c)";
  [%expect {| (a b c) |}];
  return ()
;;

let%expect_test "UTF-8" =
  let t = "┌" |> of_utf8_bytes in
  print_string (t |> to_utf8_bytes_exn);
  [%expect {| ┌ |}];
  print_s [%sexp (t : t)];
  (* renders using escapes due to [Sexp.to_string] *)
  [%expect {| "\226\148\140" |}];
  return ()
;;

let%expect_test "[sexp_of_t] on a hash table respects [Print.length]" =
  let test size =
    let htbl = Hash_table.create () in
    for i = 1 to size do
      let i = Value.of_int_exn i in
      Hash_table.set htbl ~key:i ~data:i
    done;
    Current_buffer.set_value_temporarily Sync Print.length (Some 10) ~f:(fun () ->
      show (htbl |> Hash_table.to_value))
  in
  test 1;
  [%expect
    {|
      "#s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data (1 1 ...))" |}];
  test 20;
  [%expect
    {|
    "#s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data (1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 ...))" |}];
  return ()
;;

let%expect_test "[sexp_of_t] on a vector respects [print_length]" =
  let show_vector size =
    Current_buffer.set_value_temporarily Sync Print.length (Some 10) ~f:(fun () ->
      show (Vector.of_list (List.init size ~f:Value.of_int_exn) |> Vector.to_value))
  in
  show_vector 2;
  [%expect {| "[0 1]" |}];
  show_vector 20;
  [%expect {| "[0 1 2 3 4 5 6 7 8 9 ...]" |}];
  return ()
;;

let%expect_test "[sexp_of_t] on a list respects [print_length]" =
  let test size =
    Current_buffer.set_value_temporarily Sync Print.length (Some 10) ~f:(fun () ->
      show (List.init size ~f:Fn.id |> Value.Type.(to_value (list int))))
  in
  test 2;
  [%expect {| (0 1) |}];
  test 20;
  [%expect {| (0 1 2 3 4 5 6 7 8 9 . ...) |}];
  return ()
;;

let%expect_test "[sexp_of_t] respects [print_level]" =
  let rec value i =
    if i = 0
    then Value.nil
    else (
      let v = value (i - 1) in
      Value.cons v v)
  in
  Current_buffer.set_value_temporarily Sync Print.level (Some 3) ~f:(fun () ->
    show (value 7));
  [%expect
    {|
    (((... ... ... ... ...) (... ... ... ...) (... ... ...) (... ...) (...) nil)
     ((... ... ... ...) (... ... ...) (... ...) (...) nil)
     ((... ... ...) (... ...) (...) nil)
     ((... ...) (...) nil)
     ((...) nil)
     (nil)
     nil) |}];
  return ()
;;

let%expect_test "[is_array]" =
  test_predicate is_array nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[is_buffer]" =
  test_predicate is_buffer nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[is_cons]" =
  test_predicate is_cons nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[is_float]" =
  test_predicate is_float nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[is_font]" =
  test_predicate is_font nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[is_frame]" =
  test_predicate is_frame nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[is_function]" =
  test_predicate is_function nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[is_hash_table]" =
  test_predicate is_hash_table nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[is_integer]" =
  test_predicate is_integer nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[is_keymap]" =
  test_predicate is_keymap nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[is_marker]" =
  test_predicate is_marker nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[is_nil]" =
  test_predicate is_nil nil;
  [%expect {| true |}];
  return ()
;;

let%expect_test "[is_not_nil]" =
  test_predicate is_not_nil nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[is_process]" =
  test_predicate is_process nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[is_string]" =
  test_predicate is_string nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[is_symbol]" =
  test_predicate is_symbol nil;
  [%expect {| true |}];
  return ()
;;

let%expect_test "[is_syntax_table]" =
  test_predicate is_syntax_table nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[is_window]" =
  test_predicate is_window nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[is_window_configuration]" =
  test_predicate is_window_configuration nil;
  [%expect {| false |}];
  return ()
;;

let%expect_test "[of_bool]" =
  List.iter [ true; false ] ~f:(fun bool ->
    print_s [%message (bool : bool) ~value:(bool |> of_bool : t)]);
  [%expect {|
    ((bool  true)
     (value t))
    ((bool  false)
     (value nil)) |}];
  return ()
;;

let%expect_test "[to_bool]" =
  List.iter [ t; nil; 13 |> of_int_exn ] ~f:(fun value ->
    print_s [%message (value : t) ~bool:(value |> to_bool : bool)]);
  [%expect
    {|
    ((value t)
     (bool  true))
    ((value nil)
     (bool  false))
    ((value 13)
     (bool  true)) |}];
  return ()
;;

let%expect_test "[cons]" =
  show (cons (13 |> of_int_exn) nil);
  [%expect {| (13) |}];
  show (cons (13 |> of_int_exn) (14 |> of_int_exn));
  [%expect {| (13 . 14) |}];
  return ()
;;

let%expect_test "[car_exn]" =
  show (car_exn (cons (13 |> of_int_exn) nil));
  [%expect {| 13 |}];
  return ()
;;

let%expect_test "[car_exn] raise" =
  show_raise (fun () -> car_exn (13 |> of_int_exn));
  [%expect {| (raised (wrong-type-argument (listp 13))) |}];
  return ()
;;

let%expect_test "[cdr_exn]" =
  show (cdr_exn (cons nil (13 |> of_int_exn)));
  [%expect {| 13 |}];
  return ()
;;

let%expect_test "[cdr_exn] raise" =
  show_raise (fun () -> cdr_exn (13 |> of_int_exn));
  [%expect {| (raised (wrong-type-argument (listp 13))) |}];
  return ()
;;

let%expect_test "[equal]" =
  let ts = [ t; nil; 13 |> of_int_exn; list (List.init 5 ~f:of_int_exn) ] in
  List.iter ts ~f:(fun t1 ->
    List.iter ts ~f:(fun t2 ->
      print_s
        [%message "equal" ~_:(t1 : t) ~_:(t2 : t) "-->" ~_:(equal t1 t2 : bool)]));
  [%expect
    {|
    (equal t t --> true)
    (equal t nil --> false)
    (equal t 13 --> false)
    (equal t (0 1 2 3 4) --> false)
    (equal nil t --> false)
    (equal nil nil --> true)
    (equal nil 13 --> false)
    (equal nil (0 1 2 3 4) --> false)
    (equal 13 t --> false)
    (equal 13 nil --> false)
    (equal 13 13 --> true)
    (equal 13 (0 1 2 3 4) --> false)
    (equal (0 1 2 3 4) t --> false)
    (equal (0 1 2 3 4) nil --> false)
    (equal (0 1 2 3 4) 13 --> false)
    (equal
      (0 1 2 3 4)
      (0 1 2 3 4)
      -->
      true) |}];
  return ()
;;

let%expect_test "[equal] on non-eq values" =
  let f () = cons (1 |> of_int_exn) (2 |> of_int_exn) in
  require [%here] (not (eq (f ()) (f ())));
  require [%here] (equal (f ()) (f ()));
  return ()
;;

let%expect_test "[to_list_exn]" =
  let show list = print_s [%sexp (to_list_exn list ~f:to_int_exn : int list)] in
  show nil;
  [%expect {| () |}];
  show (list (List.init 3 ~f:of_int_exn));
  [%expect {| (0 1 2) |}];
  return ()
;;

let%expect_test "[list_to_array_exn]" =
  let show list = print_s [%sexp (list_to_array_exn list ~f:to_int_exn : int array)] in
  show nil;
  [%expect {| () |}];
  show (list (List.init 3 ~f:of_int_exn));
  [%expect {| (0 1 2) |}];
  return ()
;;

let%expect_test "[to_list_exn] raise" =
  require_does_raise [%here] (fun () ->
    to_list_exn (13 |> of_int_exn) ~f:(fun _ -> assert false));
  [%expect {| ("[Value.to_list] got strange value" 13) |}];
  return ()
;;

let%expect_test "[to_float_exn] raise" =
  require_does_raise [%here] (fun () -> nil |> to_float_exn);
  [%expect {| (wrong-type-argument (floatp nil)) |}];
  return ()
;;

let%expect_test "[to_int_exn] raise" =
  require_does_raise [%here] (fun () -> nil |> to_int_exn);
  [%expect {| (wrong-type-argument (integerp nil)) |}];
  return ()
;;

let%expect_test "[to_utf8_bytes_exn] raise" =
  require_does_raise [%here] (fun () -> nil |> to_utf8_bytes_exn);
  [%expect {| (wrong-type-argument (stringp nil)) |}];
  return ()
;;

module Make_subtype = struct
  module Non_nil = Make_subtype (struct
      let name = "non-nil"
      let here = [%here]
      let is_in_subtype = is_not_nil
    end)

  open Non_nil

  let%expect_test "[of_value_exn] success" =
    let t = of_value_exn (13 |> of_int_exn) in
    print_s [%sexp (t : t)];
    [%expect {| 13 |}];
    return ()
  ;;

  let%expect_test "[of_value_exn] failure" =
    require_does_raise [%here] ~hide_positions:true (fun () -> of_value_exn nil);
    [%expect {| ("[non-nil]'s [of_value_exn] got value not in subtype" nil) |}];
    return ()
  ;;

  let%expect_test "[to_value]" =
    let v1 = 13 |> of_int_exn in
    let t = v1 |> of_value_exn in
    let v2 = t |> to_value in
    require [%here] (Value.eq v1 v2 : bool);
    return ()
  ;;

  let%expect_test "[eq] false" =
    let t13 = 13 |> of_int_exn |> of_value_exn in
    let t14 = 14 |> of_int_exn |> of_value_exn in
    require [%here] (not (eq t13 t14));
    return ()
  ;;

  let%expect_test "[eq] true" =
    let v = 13 |> of_int_exn in
    let t1 = v |> of_value_exn in
    let t2 = v |> of_value_exn in
    require [%here] (eq t1 t2 : bool);
    return ()
  ;;
end

let print_args =
  lambda
    [%here]
    (Returns Value.Type.value)
    (let%map_open.Defun () = return ()
     and args = rest "args" value in
     print_s [%message (args : t list)];
     list args)
  |> Function.to_value
;;

let%expect_test "[funcallN_i]" =
  for i = 0 to 5 do
    funcallN_i print_args (List.init i ~f:of_int_exn)
  done;
  [%expect
    {|
    (args ())
    (args (0))
    (args (0 1))
    (args (0 1 2))
    (args (0 1 2 3))
    (args (0 1 2 3 4)) |}];
  return ()
;;

let v0 = 0 |> of_int_exn
let v1 = 1 |> of_int_exn
let v2 = 2 |> of_int_exn
let v3 = 3 |> of_int_exn
let v4 = 4 |> of_int_exn

let%expect_test "[funcall{0..5}]" =
  let show result = print_s [%message (result : t)] in
  show (funcall0 print_args);
  [%expect {|
    (args ())
    (result nil) |}];
  show (funcall1 print_args v0);
  [%expect {|
    (args (0))
    (result (0)) |}];
  show (funcall2 print_args v0 v1);
  [%expect {|
    (args (0 1))
    (result (0 1)) |}];
  show (funcall3 print_args v0 v1 v2);
  [%expect {|
    (args (0 1 2))
    (result (0 1 2)) |}];
  show (funcall4 print_args v0 v1 v2 v3);
  [%expect {|
    (args (0 1 2 3))
    (result (0 1 2 3)) |}];
  show (funcall5 print_args v0 v1 v2 v3 v4);
  [%expect {|
    (args (0 1 2 3 4))
    (result (0 1 2 3 4)) |}];
  return ()
;;

let%expect_test "[funcall{0..5}_i]" =
  funcall0_i print_args;
  [%expect {| (args ()) |}];
  funcall1_i print_args v0;
  [%expect {| (args (0)) |}];
  funcall2_i print_args v0 v1;
  [%expect {| (args (0 1)) |}];
  funcall3_i print_args v0 v1 v2;
  [%expect {| (args (0 1 2)) |}];
  funcall4_i print_args v0 v1 v2 v3;
  [%expect {| (args (0 1 2 3)) |}];
  funcall5_i print_args v0 v1 v2 v3 v4;
  [%expect {| (args (0 1 2 3 4)) |}];
  return ()
;;

let%expect_test "[funcall_int_int_value_unit]" =
  funcall_int_int_value_unit print_args 0 1 v2;
  [%expect {| (args (0 1 2)) |}];
  return ()
;;

let%expect_test "[funcall_int_int_value_value_unit]" =
  funcall_int_int_value_value_unit print_args 0 1 v2 v3;
  [%expect {| (args (0 1 2 3)) |}];
  return ()
;;

let print_num_args =
  lambda
    [%here]
    (Returns Value.Type.value)
    (let%map_open.Defun () = return ()
     and args = rest "args" value in
     print_s [%message "" ~num_args:(List.length args : int)];
     list args)
  |> Function.to_value
;;

let length = Funcall.Wrap.("length" <: value @-> return value)

let%expect_test "[funcallN_array{,_i}] with many arguments" =
  let array = Array.create nil ~len:3_500_000 in
  let list = funcallN_array print_num_args array in
  [%expect {| (num_args 3_500_000) |}];
  let length = length list in
  print_s [%sexp (length : t)];
  [%expect {| 3500000 |}];
  funcallN_array_i print_num_args array;
  [%expect {| (num_args 3_500_000) |}];
  return ()
;;

let%expect_test "Elisp throw translated into OCaml and back" =
  let sexp_of_throw = function
    | Elisp_throw { tag; value } -> [%sexp "Elisp_throw", { tag : t; value : t }]
    | exn -> [%sexp "Not a throw", (exn : exn)]
  in
  let fn =
    lambda_nullary_nil [%here] (fun () ->
      (* Catching an Elisp_throw here checks that throws are translated from elisp to
         ecaml correctly. *)
      try Funcall.Wrap.("top-level" <: nullary @-> return nil) () with
      | exn ->
        print_s
          [%sexp
            "Inner Ecaml function caught exception, reraising"
          , (sexp_of_throw exn : Sexp.t)];
        (* Raising the throw back to elisp and catching it below checks that throws are
           translated from ecaml to elisp correctly. *)
        raise exn)
  in
  let form =
    Form.(list [ symbol (Symbol.intern "funcall"); quote (Function.to_value fn) ])
  in
  (try Form.Blocking.eval_i form with
   | exn ->
     print_s [%sexp "Outer Ecaml function caught exception", (sexp_of_throw exn : Sexp.t)]);
  [%expect
    {|
    ("Inner Ecaml function caught exception, reraising"
     (Elisp_throw (
       (tag   top-level)
       (value nil))))
    ("Outer Ecaml function caught exception" (
      Elisp_throw (
        (tag   top-level)
        (value nil)))) |}];
  return ()
;;

let%expect_test "[catch]ing a [throw] that travels through Ecaml" =
  defun
    ("some-ecaml-function" |> Symbol.intern)
    [%here]
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open f = required "f" value in
     Value.funcall0_i f);
  let value =
    Form.Blocking.eval_string
      {|
(catch 'some-tag
  (some-ecaml-function (lambda () (throw 'some-tag 13))))
|}
  in
  print_s [%sexp (value : Value.t)];
  [%expect {| 13 |}];
  return ()
;;

let%expect_test "rendering OCaml exceptions in Emacs and Ocaml" =
  let test ?(debug_on_error = false) message =
    try
      Current_buffer.set_value_temporarily
        Sync
        (Debugger.debug_on_error |> Customization.var)
        debug_on_error
        ~f:(fun () ->
          ignore
            (Value.funcall1
               ("(lambda (f) (funcall f))" |> Form.Blocking.eval_string)
               (lambda_nullary_nil [%here] (fun () -> raise_s message)
                |> Function.to_value)
             : Value.t));
      assert false
    with
    | exn ->
      print_endline "Ocaml:";
      print_s [%sexp (exn : exn)];
      print_endline "Emacs:";
      (match exn with
       | Elisp_signal { data; _ } ->
         (match to_list_exn data ~f:Fn.id with
          | [] -> assert false
          | string :: rest ->
            let string = string |> to_utf8_bytes_exn in
            if List.is_empty rest
            then print_endline string
            else
              (* The following format is based on how Emacs renders errors in the
                 minibuffer.  E.g. [M-x eval-expression]:

                 {v
                   (defun test-error-rendering ()
                    (interactive)
                    (signal 'error '("foo" (a b) c d)))
                 v}

                 And then [M-x test-error-rendering] will output:

                 {v
                   foo: (a b), c, d
                 v} *)
              printf
                "%s: %s\n"
                string
                (concat ~sep:", " (List.map rest ~f:prin1_to_string)))
       | _ -> require [%here] false)
  in
  test [%message "foo"];
  [%expect {|
    Ocaml:
    (foo)
    Emacs:
    foo |}];
  test [%message "foo"] ~debug_on_error:true;
  [%expect
    {|
    Ocaml:
    ((foo (backtrace ("<backtrace elided in test>"))))
    Emacs:
    (foo (backtrace ("<backtrace elided in test>"))) |}];
  test [%message "foo" "bar"];
  [%expect {|
    Ocaml:
    ((foo bar))
    Emacs:
    (foo bar) |}];
  test [%message "foo" "bar"] ~debug_on_error:true;
  [%expect
    {|
    Ocaml:
    (((foo bar) (backtrace ("<backtrace elided in test>"))))
    Emacs:
    ((foo bar) (backtrace ("<backtrace elided in test>"))) |}];
  test (List (List.init 15 ~f:(fun i -> Sexp.Atom ("foobar" ^ Int.to_string i))));
  [%expect
    {|
    Ocaml:
    ((
      foobar0
      foobar1
      foobar2
      foobar3
      foobar4
      foobar5
      foobar6
      foobar7
      foobar8
      foobar9
      foobar10
      foobar11
      foobar12
      foobar13
      foobar14))
    Emacs:
    (foobar0 foobar1 foobar2 foobar3 foobar4 foobar5 foobar6 foobar7 foobar8
     foobar9 foobar10 foobar11 foobar12 foobar13 foobar14) |}];
  return ()
;;

module Type = struct
  open Type

  let%expect_test "[sexp_of_t]" =
    print_s [%sexp (int : _ t)];
    [%expect {| int |}];
    print_s [%sexp (list int : _ t)];
    [%expect {| (list int) |}];
    return ()
  ;;

  let%expect_test "round trip" =
    let test a t =
      let v1 = a |> Value.Type.to_value t in
      let a2 = v1 |> Value.Type.of_value_exn t in
      let v2 = a2 |> Value.Type.to_value t in
      require [%here] (Value.equal v1 v2);
      print_s [%message (v1 : Value.t) (v2 : Value.t)]
    in
    test true bool;
    [%expect {|
      ((v1 t)
       (v2 t)) |}];
    test false bool;
    [%expect {|
      ((v1 nil)
       (v2 nil)) |}];
    test 13 int;
    [%expect {|
      ((v1 13)
       (v2 13)) |}];
    test "foo" string;
    [%expect {|
      ((v1 foo)
       (v2 foo)) |}];
    test None (nil_or int);
    [%expect {|
      ((v1 nil)
       (v2 nil)) |}];
    test (Some 13) (nil_or int);
    [%expect {|
      ((v1 13)
       (v2 13)) |}];
    test None (option int);
    [%expect {|
      ((v1 nil)
       (v2 nil)) |}];
    test (Some 13) (option int);
    [%expect {|
      ((v1 (13))
       (v2 (13))) |}];
    test None (option bool);
    [%expect {|
      ((v1 nil)
       (v2 nil)) |}];
    test (Some false) (option bool);
    [%expect {|
      ((v1 (nil))
       (v2 (nil))) |}];
    test [] (list int);
    [%expect {|
      ((v1 nil)
       (v2 nil)) |}];
    test [ 13; 14 ] (list int);
    [%expect {|
      ((v1 (13 14))
       (v2 (13 14))) |}];
    test [ [ "foo" ] ] (list (list string));
    [%expect {|
      ((v1 ((foo)))
       (v2 ((foo)))) |}];
    return ()
  ;;
end
