open! Core_kernel
open! Import
open! Ansi_color
open! Test_ansi_color

let%expect_test "two escape sequences" =
  for code1 = 0 to max_supported_code do
    for code2 = 0 to max_supported_code do
      test (concat [ escape [ code1 ]; "a"; escape [ code2 ]; "b" ]);
    done;
  done;
  [%expect {| |}];
;;
