open! Core_kernel
open! Import
open! Async
open! Async_testing

let%expect_test "test basic async" =
  let%bind () = Clock.after (Time.Span.of_sec 0.1) in
  printf "Hello world\n";
  [%expect {| Hello world |}]
;;

let%expect_test "disk IO" =
  let temp_file = File.make_temp_file ~prefix:"test" ~suffix:"test" in
  let%bind () =
    Writer.with_file temp_file ~f:(fun writer ->
      Writer.write writer "Hello world";
      Deferred.unit)
  in
  let%bind () =
    Reader.with_file temp_file ~f:(fun reader ->
      let%map line = Reader.read_line reader in
      print_s [%sexp (line : string Reader.Read_result.t)])
  in
  [%expect {| (Ok "Hello world") |}]
;;
