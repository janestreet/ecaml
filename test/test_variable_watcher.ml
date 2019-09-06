open! Core_kernel
open! Async_kernel
open! Import
open! Variable_watcher

let var =
  defvar
    ("watch-this-variable" |> Symbol.intern)
    [%here]
    ~docstring:"test variable"
    ~type_:Value.Type.int
    ~initial_value:0
    ()
;;

let%expect_test _ =
  let watcher = add [%here] var ~f:(fun event -> message_s [%sexp (event : Event.t)]) in
  Current_buffer.set_value var 3;
  [%expect
    {|
      ((local_to_buffer ()) (new_value 3) (operation Set)
       (variable_changed watch-this-variable)) |}];
  Current_buffer.set_value var 6;
  [%expect
    {|
      ((local_to_buffer ()) (new_value 6) (operation Set)
       (variable_changed watch-this-variable)) |}];
  remove watcher;
  Current_buffer.set_value var 8;
  [%expect {||}];
  return ()
;;
