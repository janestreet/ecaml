open! Core_kernel
open! Async_kernel
open! Import
open! Bookmark

let%expect_test "[of_string], [sexp_of_t]" =
  let t = of_string "some-bookmark" in
  print_s [%sexp (t : t)];
  [%expect {| some-bookmark |}];
  return ()
;;

let%expect_test "[set]" =
  let t = of_string "some-bookmark" in
  set t (Map.empty (module Symbol.Compare_name)) ~no_overwrite:true;
  return ()
;;

let%expect_test "[Make_record_function.in_buffer]" =
  print_s [%sexp (Make_record_function.in_buffer : Symbol.t Buffer_local.t)];
  [%expect {| (bookmark-make-record-function symbol) |}];
  print_s
    [%sexp (Current_buffer.get_buffer_local Make_record_function.in_buffer : Symbol.t)];
  [%expect {| bookmark-make-record-default |}];
  return ()
;;
