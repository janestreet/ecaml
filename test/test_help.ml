open! Core_kernel
open! Async_kernel
open! Import

let%expect_test "where-is" =
  let symbol = "dummy-function" |> Symbol.intern in
  defun_nullary_nil symbol [%here] ~interactive:No_arg (fun () -> ());
  let define sequence =
    Keymap.(define_key (global ()) (Key_sequence.create_exn sequence) (Symbol symbol))
  in
  let show () = print_s [%sexp (Help.where_is symbol : Key_sequence.t option)] in
  show ();
  [%expect {| () |}];
  define "C-c M-t M-e M-s M-t C-_ M-!";
  show ();
  [%expect {| ("C-c M-t M-e M-s M-t C-_ M-!") |}];
  (* [where_is] returns the shortest available key sequence. *)
  define "C-c s";
  show ();
  [%expect {| ("C-c s") |}];
  return ()
;;
