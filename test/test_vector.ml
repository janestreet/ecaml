open! Core_kernel
open! Import
open! Vector

let show t = print_s [%sexp (t : t)]

let%expect_test "[create] raise" =
  show_raise (fun () -> create ~len:(-1) Value.nil);
  [%expect {|
    (raised (wrong-type-argument (wholenump -1))) |}];
;;

let%expect_test "[create]" =
  for len = 0 to 3 do
    show (create ~len Value.nil);
  done;
  [%expect {|
    []
    [nil]
    "[nil nil]"
    "[nil nil nil]" |}];
;;

let%expect_test "[of_list]" =
  for len = 0 to 3 do
    show (of_list (List.init len ~f:Value.of_int_exn));
  done;
  [%expect {|
    []
    [0]
    "[0 1]"
    "[0 1 2]" |}];
;;

let%expect_test "[get] raise" =
  let t = of_list [] in
  let test i = show_raise (fun () -> get t i) in
  test (-1);
  [%expect {|
    (raised (
      "[Vector.get] got invalid subscript"
      (subscript -1)
      (length    0)
      (vector    []))) |}];
  test 0;
  [%expect {|
    (raised (
      "[Vector.get] got invalid subscript"
      (subscript 0)
      (length    0)
      (vector    []))) |}];
;;

let%expect_test "[get]" =
  for len = 1 to 4 do
    let t = of_list (List.init len ~f:Value.of_int_exn) in
    print_s [%sexp (List.init len ~f:(get t) : Value.t list)];
  done;
  [%expect {|
    (0)
    (0 1)
    (0 1 2)
    (0 1 2 3) |}];
;;

let%expect_test "[set] raise on empty vector" =
  let t = of_list [] in
  let test i = show_raise (fun () -> set t i Value.nil) in
  test (-1);
  [%expect {|
    (raised (
      "[Vector.set] got invalid subscript"
      (subscript -1)
      (length    0)
      (vector    []))) |}];
  test 0;
  [%expect {|
    (raised (
      "[Vector.set] got invalid subscript"
      (subscript 0)
      (length    0)
      (vector    []))) |}];
;;

let%expect_test "[set] raise on non-empty vector" =
  let t = of_list [ 13 |> Value.of_int_exn ] in
  let test i = show_raise (fun () -> set t i Value.nil) in
  test (-1);
  [%expect {|
    (raised (
      "[Vector.set] got invalid subscript"
      (subscript -1)
      (length    1)
      (vector    [13]))) |}];
  test 1;
  [%expect {|
    (raised (
      "[Vector.set] got invalid subscript"
      (subscript 1)
      (length    1)
      (vector    [13]))) |}];
;;

let%expect_test "[set]" =
  let len = 4 in
  let t = create ~len (13 |> Value.of_int_exn) in
  for i = 0 to len - 1 do
    set t i Value.nil;
    show t;
  done;
  [%expect {|
    "[nil 13 13 13]"
    "[nil nil 13 13]"
    "[nil nil nil 13]"
    "[nil nil nil nil]" |}]
;;

let%expect_test "[concat]" =
  for i = 0 to 5 do
    show (concat (List.init i ~f:(fun len -> create ~len (len |> Value.of_int_exn))));
  done;
  [%expect {|
    []
    []
    [1]
    "[1 2 2]"
    "[1 2 2 3 3 3]"
    "[1 2 2 3 3 3 4 4 4 4]" |}];
;;

let%expect_test "[to_array]" =
  for n = 0 to 3 do
    print_s [%sexp (List.init n ~f:Value.of_int_exn
                    |> of_list
                    |> to_array ~f:Value.to_int_exn
                    : int array)];
  done;
  [%expect {|
    ()
    (0)
    (0 1)
    (0 1 2) |}];
;;
