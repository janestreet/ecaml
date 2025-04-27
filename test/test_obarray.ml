open! Core
open! Async_kernel
open! Import
open! Obarray

let%expect_test "[iter], [standard]" =
  let all = ref [] in
  iter standard ~f:(fun s -> all := Symbol.name s :: !all);
  (* We ignore the last three digits of the length to reduce noise. *)
  print_s [%sexp (List.length !all / 1000 * 1000 : int)];
  [%expect {| 19_000 |}];
  return ()
;;
