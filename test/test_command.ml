open! Core_kernel
open! Import
open! Command

let is_command value = print_s [%sexp (Value.is_command value : bool)]

let%expect_test "[Raw_prefix_argument]" =
  let f = Symbol.gensym () in
  defun [%here] Value.Type.unit f ~interactive:"P"
    (let open Defun.Let_syntax in
     let%map_open arg = required ("arg" |> Symbol.intern) Value.Type.value in
     print_s [%message
       ""
         (arg : Value.t)
         ~for_current_command:(
           Current_buffer.value_exn Raw_prefix_argument.for_current_command
           : Raw_prefix_argument.t)
         ~raw_prefix_argument:(
           arg |> Raw_prefix_argument.of_value_exn : Raw_prefix_argument.t)]);
  is_command (f |> Symbol.to_value);
  [%expect {|
    true |}];
  List.iter
    [ Value.nil
    ; 3 |> Value.of_int_exn
    ; Value.list [ 4 |> Value.of_int_exn ]
    ; "-" |> Value.intern ]
    ~f:(fun prefix_arg ->
      print_s [%message (prefix_arg : Value.t)];
      Command.call_interactively (f |> Symbol.to_value)
        (prefix_arg |> Raw_prefix_argument.of_value_exn));
  [%expect {|
    (prefix_arg nil)
    ((arg                 nil)
     (for_current_command Absent)
     (raw_prefix_argument Absent))
    (prefix_arg 3)
    ((arg 3)
     (for_current_command (Int 3))
     (raw_prefix_argument (Int 3)))
    (prefix_arg (4))
    ((arg (4))
     (for_current_command (Nested 4))
     (raw_prefix_argument (Nested 4)))
    (prefix_arg -)
    ((arg                 -)
     (for_current_command Minus)
     (raw_prefix_argument Minus)) |}]
;;
