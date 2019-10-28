open! Core_kernel
open! Async_kernel
open! Import
open! Hash_table

let show t = print_s [%sexp (t : t)]
let key = "foo" |> Value.of_utf8_bytes
let data = "foo-value" |> Value.of_utf8_bytes

let%expect_test "[create]" =
  show (create ());
  [%expect
    {|
   "#s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data ())" |}];
  return ()
;;

let%expect_test "[find], [set], [remove]" =
  let t = create () in
  print_s [%sexp (find t key : Value.t option)];
  [%expect {| () |}];
  set t ~key ~data;
  print_s [%sexp (find t key : Value.t option)];
  [%expect {| (foo-value) |}];
  remove t key;
  print_s [%sexp (find t key : Value.t option)];
  [%expect {| () |}];
  return ()
;;

let%expect_test "[keys]" =
  let t = create () in
  print_s [%sexp (keys t : Value.t list)];
  [%expect {| () |}];
  set t ~key ~data;
  print_s [%sexp (keys t : Value.t list)];
  [%expect {| (foo) |}];
  return ()
;;

let%expect_test "[length]" =
  let t = create () in
  print_s [%sexp (length t : int)];
  [%expect {| 0 |}];
  set t ~key ~data;
  print_s [%sexp (length t : int)];
  [%expect {| 1 |}];
  return ()
;;

let%expect_test "hashtable tests" =
  let test test =
    let t = create ~test () in
    set t ~key ~data;
    print_s [%sexp "find using physically equal key", (find t key : Value.t option)];
    print_s
      [%sexp
        "find using physically nonequal key"
      , (find t (key |> Value.to_utf8_bytes_exn |> Value.of_utf8_bytes)
         : Value.t option)]
  in
  test Eq;
  [%expect
    {|
    ("find using physically equal key" (foo-value))
    ("find using physically nonequal key" ()) |}];
  test Eql;
  [%expect
    {|
    ("find using physically equal key" (foo-value))
    ("find using physically nonequal key" ()) |}];
  test Equal;
  [%expect
    {|
    ("find using physically equal key" (foo-value))
    ("find using physically nonequal key" (foo-value)) |}];
  return ()
;;
