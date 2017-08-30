open! Core_kernel
open! Import
open! Hash_table

let show t = print_s [%sexp (t : t)]

let%expect_test "[create]" =
  show (create ());
  [%expect {|
    "#s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8 data ())" |}];
;;
