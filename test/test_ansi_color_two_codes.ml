open! Core_kernel
open! Import
open! Ansi_color
open! Test_ansi_color

let%expect_test "double codes" =
  for code1 = 0 to max_supported_code do
    for code2 = 0 to max_supported_code do
      test_codes [ code1; code2 ];
    done;
  done;
  [%expect {| |}];
;;
