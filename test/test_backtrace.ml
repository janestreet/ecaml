open! Core_kernel
open! Async_kernel
open! Import

(* We don't want to capture, e.g., the test directory, which is a jenga sandbox tempdir,
   or the command line arguments to the test runner, which depend on user config.

   It's okay for us to mangle the backtrace quite badly in tests, since we only need to
   prove that we're getting back the thing emacs calls a backtrace.
*)
let omit_args_for_determinism backtrace =
  Parsexp.Many.parse_string_exn backtrace
  |> List.filter_map ~f:(function
    | List _ -> None
    | Atom s -> Some s)
  |> String.concat ~sep:"\n"
;;

let%expect_test _ =
  Emacs_backtrace.get () |> omit_args_for_determinism |> print_endline;
  [%expect
    {|
      backtrace
      load
      command-line-1
      command-line
      normal-top-level |}];
  return ()
;;
