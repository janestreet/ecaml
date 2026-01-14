open! Core
open! Async_kernel
open! Import

let%expect_test _ =
  (* Most of the frames have test directory strings as arguments; just grab the last two
     to prove this is a backtrace. *)
  List.take (Emacs_backtrace.get () |> String.split_lines |> List.rev) 2
  |> List.rev
  |> String.concat ~sep:"\n"
  |> print_endline;
  [%expect
    {|
    command-line()
    normal-top-level()
    |}];
  return ()
;;
