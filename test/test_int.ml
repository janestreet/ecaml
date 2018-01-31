open! Core_kernel
open! Import

let eval_int_var string = Form.eval (string |> Symbol.intern |> Form.symbol)

let most_negative_fixnum_value = eval_int_var "most-negative-fixnum"
let most_positive_fixnum_value = eval_int_var "most-positive-fixnum"

let most_negative_fixnum = Value.emacs_min_int
let most_positive_fixnum = Value.emacs_max_int

let%expect_test "most-{neg,pos}itive-fixnum" =
  print_s [%sexp (most_negative_fixnum_value : Value.t)];
  [%expect {|
    -2305843009213693952 |}];
  print_s [%sexp (most_negative_fixnum : int)];
  [%expect {|
    -2_305_843_009_213_693_952 |}];
  print_s [%sexp (most_positive_fixnum_value : Value.t)];
  [%expect {|
    2305843009213693951 |}];
  print_s [%sexp (most_positive_fixnum : int)];
  [%expect {|
    2_305_843_009_213_693_951 |}];
;;

let%expect_test "[Value.of_int]" =
  List.iter
    [ Int.min_value
    ; Int.min_value + 1
    ; most_negative_fixnum - 1
    ; most_negative_fixnum
    ; most_negative_fixnum + 1
    ; -1
    ; 0
    ; 1
    ; most_positive_fixnum - 1
    ; most_positive_fixnum
    ; most_positive_fixnum + 1
    ; Int.max_value - 1
    ; Int.max_value
    ]
    ~f:(fun i ->
      let v = Or_error.try_with (fun () ->
        require_no_allocation [%here] (fun () -> Value.of_int_exn i))
      in
      let i' =
        match v with
        | Ok v ->
          require [%here] (Obj.is_int (Obj.repr v));
          Or_error.try_with (fun () -> Value.to_int_exn v)
        | Error _ as x -> x
      in
      print_s [%message
        ""
          (i : int)
          (v : Value.t Or_error.t)
          (i' : int Or_error.t)];
      if most_negative_fixnum <= i && i <= most_positive_fixnum
      then (require [%here] (match i' with  Ok i' -> i = i' | Error _ -> false)));
  [%expect {|
    ((i -4_611_686_018_427_387_904)
     (v (
       Error (
         "validation errors" ((
           overflow-error
           "value -4611686018427387904 < bound -2305843009213693952")))))
     (i' (
       Error (
         "validation errors" ((
           overflow-error
           "value -4611686018427387904 < bound -2305843009213693952"))))))
    ((i -4_611_686_018_427_387_903)
     (v (
       Error (
         "validation errors" ((
           overflow-error
           "value -4611686018427387903 < bound -2305843009213693952")))))
     (i' (
       Error (
         "validation errors" ((
           overflow-error
           "value -4611686018427387903 < bound -2305843009213693952"))))))
    ((i -2_305_843_009_213_693_953)
     (v (
       Error (
         "validation errors" ((
           overflow-error
           "value -2305843009213693953 < bound -2305843009213693952")))))
     (i' (
       Error (
         "validation errors" ((
           overflow-error
           "value -2305843009213693953 < bound -2305843009213693952"))))))
    ((i -2_305_843_009_213_693_952)
     (v  (Ok -2305843009213693952))
     (i' (Ok -2_305_843_009_213_693_952)))
    ((i -2_305_843_009_213_693_951)
     (v  (Ok -2305843009213693951))
     (i' (Ok -2_305_843_009_213_693_951)))
    ((i -1)
     (v  (Ok -1))
     (i' (Ok -1)))
    ((i 0)
     (v  (Ok 0))
     (i' (Ok 0)))
    ((i 1)
     (v  (Ok 1))
     (i' (Ok 1)))
    ((i 2_305_843_009_213_693_950)
     (v  (Ok 2305843009213693950))
     (i' (Ok 2_305_843_009_213_693_950)))
    ((i 2_305_843_009_213_693_951)
     (v  (Ok 2305843009213693951))
     (i' (Ok 2_305_843_009_213_693_951)))
    ((i 2_305_843_009_213_693_952)
     (v (
       Error (
         "validation errors" ((
           overflow-error "value 2305843009213693952 > bound 2305843009213693951")))))
     (i' (
       Error (
         "validation errors" ((
           overflow-error "value 2305843009213693952 > bound 2305843009213693951"))))))
    ((i 4_611_686_018_427_387_902)
     (v (
       Error (
         "validation errors" ((
           overflow-error "value 4611686018427387902 > bound 2305843009213693951")))))
     (i' (
       Error (
         "validation errors" ((
           overflow-error "value 4611686018427387902 > bound 2305843009213693951"))))))
    ((i 4_611_686_018_427_387_903)
     (v (
       Error (
         "validation errors" ((
           overflow-error "value 4611686018427387903 > bound 2305843009213693951")))))
     (i' (
       Error (
         "validation errors" ((
           overflow-error "value 4611686018427387903 > bound 2305843009213693951")))))) |}];
;;

let%expect_test "ints coming from emacs are not boxed" =
  let v = Form.eval (Form.read "123456") in
  print_s [%message "Value" (v : Value.t)
                      ~boxed:(Obj.is_block (Obj.repr v) : bool)];
  require [%here] (Value.to_int_exn v = 123_456);
  [%expect {|
    (Value
      (v     123456)
      (boxed false)) |}];
;;

