open! Core_kernel
open! Import
open! Value

let show t = print_s [%sexp (t : t)]

let test_predicate f t = print_s [%sexp (f t : bool)]

let%expect_test "UTF-8" =
  let t = "┌" |> of_utf8_bytes in
  print_string (t |> to_utf8_bytes_exn);
  [%expect {| ┌ |}];
  print_s [%sexp (t : t)];
  [%expect {| "\226\148\140" |}]; (* renders using escapes due to [Sexp.to_string] *)
;;

let%expect_test "[is_array]" =
  test_predicate is_array nil;
  [%expect {|
    false |}];
;;

let%expect_test "[is_buffer]" =
  test_predicate is_buffer nil;
  [%expect {|
    false |}];
;;

let%expect_test "[is_cons]" =
  test_predicate is_cons nil;
  [%expect {|
    false |}];
;;

let%expect_test "[is_float]" =
  test_predicate is_float nil;
  [%expect {|
    false |}];
;;

let%expect_test "[is_font]" =
  test_predicate is_font nil;
  [%expect {|
    false |}];
;;

let%expect_test "[is_frame]" =
  test_predicate is_frame nil;
  [%expect {|
    false |}];
;;

let%expect_test "[is_function]" =
  test_predicate is_function nil;
  [%expect {|
    false |}];
;;

let%expect_test "[is_hash_table]" =
  test_predicate is_hash_table nil;
  [%expect {|
    false |}];
;;

let%expect_test "[is_integer]" =
  test_predicate is_integer nil;
  [%expect {|
    false |}];
;;

let%expect_test "[is_keymap]" =
  test_predicate is_keymap nil;
  [%expect {|
    false |}];
;;

let%expect_test "[is_marker]" =
  test_predicate is_marker nil;
  [%expect {|
    false |}];
;;

let%expect_test "[is_nil]" =
  test_predicate is_nil nil;
  [%expect {|
    true |}];
;;

let%expect_test "[is_not_nil]" =
  test_predicate is_not_nil nil;
  [%expect {|
    false |}];
;;

let%expect_test "[is_process]" =
  test_predicate is_process nil;
  [%expect {|
    false |}];
;;

let%expect_test "[is_string]" =
  test_predicate is_string nil;
  [%expect {|
    false |}];
;;

let%expect_test "[is_symbol]" =
  test_predicate is_symbol nil;
  [%expect {|
    true |}];
;;

let%expect_test "[is_syntax_table]" =
  test_predicate is_syntax_table nil;
  [%expect {|
    false |}];
;;

let%expect_test "[is_window]" =
  test_predicate is_window nil;
  [%expect {|
    false |}]
;;

let%expect_test "[is_window_configuration]" =
  test_predicate is_window_configuration nil;
  [%expect {|
    false |}];
;;

let%expect_test "[of_bool]" =
  List.iter [ true; false ] ~f:(fun bool ->
    print_s [%message (bool : bool) ~value:(bool |> of_bool : t)]);
  [%expect {|
    ((bool  true)
     (value t))
    ((bool  false)
     (value nil)) |}];
;;

let%expect_test "[to_bool]" =
  List.iter [ t; nil; 13 |> of_int_exn ] ~f:(fun value ->
    print_s [%message (value : t) ~bool:(value |> to_bool : bool)]);
  [%expect {|
    ((value t)
     (bool  true))
    ((value nil)
     (bool  false))
    ((value 13)
     (bool  true)) |}];
;;

let%expect_test "[cons]" =
  show (cons (13 |> of_int_exn) nil);
  [%expect {|
    (13) |}];
  show (cons (13 |> of_int_exn) (14 |> of_int_exn));
  [%expect {|
    (13 . 14) |}];
;;

let%expect_test "[car_exn]" =
  show (car_exn (cons (13 |> of_int_exn) nil));
  [%expect {|
    13 |}];
;;

let%expect_test "[car_exn] raise" =
  show_raise (fun () -> car_exn (13 |> of_int_exn));
  [%expect {|
    (raised (wrong-type-argument (listp 13))) |}];
;;

let%expect_test "[cdr_exn]" =
  show (cdr_exn (cons nil (13 |> of_int_exn)));
  [%expect {|
    13 |}];
;;

let%expect_test "[cdr_exn] raise" =
  show_raise (fun () -> cdr_exn (13 |> of_int_exn));
  [%expect {|
    (raised (wrong-type-argument (listp 13))) |}];
;;

let%expect_test "[equal]" =
  let ts =
    [ t
    ; nil
    ; 13 |> of_int_exn
    ; list (List.init 5 ~f:of_int_exn) ] in
  List.iter ts ~f:(fun t1 ->
    List.iter ts ~f:(fun t2 ->
      print_s [%message
        "equal"
          ~_:(t1 : t)
          ~_:(t2 : t)
          "-->"
          ~_:(equal t1 t2 : bool)]));
  [%expect {|
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
;;

let%expect_test "[equal] on non-eq values" =
  let f () = cons (1 |> of_int_exn) (2 |> of_int_exn) in
  require [%here] (not (eq (f ()) (f ())));
  require [%here] (equal (f ()) (f ()));
;;

let%expect_test "[to_list_exn]" =
  let show list = print_s [%sexp (to_list_exn list ~f:to_int_exn : int list)] in
  show nil;
  [%expect {|
    () |}];
  show (list (List.init 3 ~f:of_int_exn));
  [%expect {|
    (0 1 2) |}];
;;

let%expect_test "[to_list_exn] raise" =
  require_does_raise [%here] (fun () ->
    to_list_exn (13 |> of_int_exn) ~f:(fun _ -> assert false));
  [%expect {|
    ("[Value.to_list] got strange value" 13) |}];
;;

let%expect_test "[to_float_exn] raise" =
  require_does_raise [%here] (fun () -> nil |> to_float_exn);
  [%expect {|
    (wrong-type-argument (floatp nil)) |}];
;;

let%expect_test "[to_int_exn] raise" =
  require_does_raise [%here] (fun () -> nil |> to_int_exn);
  [%expect {|
    (wrong-type-argument (integerp nil)) |}];
;;

let%expect_test "[to_utf8_bytes_exn] raise" =
  require_does_raise [%here] (fun () -> nil |> to_utf8_bytes_exn);
  [%expect {|
    (wrong-type-argument (stringp nil)) |}];
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
    [%expect {|
    13 |}];
  ;;

  let%expect_test "[of_value_exn] failure" =
    require_does_raise [%here] ~hide_positions:true (fun () -> of_value_exn nil);
    [%expect {|
    ("[non-nil]'s [of_value_exn] got value not in subtype" nil) |}];
  ;;

  let%expect_test "[to_value]" =
    let v1 = 13 |> of_int_exn in
    let t = v1 |> of_value_exn in
    let v2 = t |> to_value  in
    require [%here] (Value.eq v1 v2 : bool);
  ;;

  let%expect_test "[eq] false" =
    let t13 = 13 |> of_int_exn |> of_value_exn in
    let t14 = 14 |> of_int_exn |> of_value_exn in
    require [%here] (not (eq t13 t14));
  ;;

  let%expect_test "[eq] true" =
    let v = 13 |> of_int_exn in
    let t1 = v |> of_value_exn in
    let t2 = v |> of_value_exn in
    require [%here] (eq t1 t2 : bool);
  ;;
end

let print_args =
  Function.create [%here] ~args:[] ~rest_arg:("args" |> Symbol.intern)
    (fun args ->
       print_s [%message (args : t array)];
       list (args |> Array.to_list))
  |> Function.to_value
;;

let%expect_test "[funcallN_i]" =
  for i = 0 to 5 do
    funcallN_i print_args (List.init i ~f:of_int_exn);
  done;
  [%expect {|
    (args ())
    (args (0))
    (args (0 1))
    (args (0 1 2))
    (args (0 1 2 3))
    (args (0 1 2 3 4)) |}]
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
;;

let%expect_test "[funcall{0..5}_i]" =
  funcall0_i print_args;
  [%expect {| (args ()) |}];
  funcall1_i print_args v0;
  [%expect {|
    (args (0)) |}];
  funcall2_i print_args v0 v1;
  [%expect {|
    (args (0 1)) |}];
  funcall3_i print_args v0 v1 v2;
  [%expect {|
    (args (0 1 2)) |}];
  funcall4_i print_args v0 v1 v2 v3;
  [%expect {|
    (args (0 1 2 3)) |}];
  funcall5_i print_args v0 v1 v2 v3 v4;
  [%expect {|
    (args (0 1 2 3 4)) |}];
;;

let%expect_test "[funcall_int_int_value_unit]" =
  funcall_int_int_value_unit print_args 0 1 v2;
  [%expect {|
    (args (0 1 2)) |}];
;;

let%expect_test "[funcall_int_int_value_value_unit]" =
  funcall_int_int_value_value_unit print_args 0 1 v2 v3;
  [%expect {|
    (args (0 1 2 3)) |}];
;;

let print_num_args =
  Function.create [%here] ~args:[] ~rest_arg:("args" |> Symbol.intern)
    (fun args ->
       print_s [%message "" ~num_args:(Array.length args : int)];
       list (args |> Array.to_list))
  |> Function.to_value
;;

let%expect_test "[funcallN_array{,_i}] with many arguments" =
  let array = Array.create nil ~len:3_500_000 in
  let list = funcallN_array print_num_args array in
  [%expect {|
    (num_args 3_500_000) |}];
  let length = Symbol.funcall1 Q.length list in
  print_s [%sexp (length : t)];
  [%expect {|
    3500000 |}];
  funcallN_array_i print_num_args array;
  [%expect {|
    (num_args 3_500_000) |}];
;;

module Type = struct
  open Type

  let%expect_test "[sexp_of_t]" =
    print_s [%sexp (int : _ t)];
    [%expect {|
      int |}];
    print_s [%sexp (list int : _ t)];
    [%expect {|
      (list int) |}];
  ;;

  let%expect_test "round trip" =
    let test a t =
      let v1 = a |> t.to_value in
      let a2 = v1 |> t.of_value_exn in
      let v2 = a2 |> t.to_value in
      require [%here] (Value.equal v1 v2);
      print_s [%message (v1 : Value.t) (v2 : Value.t)] in
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
    test [] (list int);
    [%expect {|
      ((v1 nil)
       (v2 nil)) |}];
    test [ 13; 14 ] (list int);
    [%expect {|
      ((v1 (13 14))
       (v2 (13 14))) |}];
    test [ [ "foo" ]] (list (list string));
    [%expect {|
      ((v1 ((foo)))
       (v2 ((foo)))) |}];
  ;;
end
