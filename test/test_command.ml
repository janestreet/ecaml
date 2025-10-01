open! Core
open! Async_kernel
open! Import
open! Command

let%expect_test "[Raw_prefix_argument]" =
  List.iter
    [ Value.nil
    ; 3 |> Value.of_int_exn
    ; Value.list [ 4 |> Value.of_int_exn ]
    ; "-" |> Value.intern
    ]
    ~f:(fun prefix_arg ->
      print_s
        [%message
          ""
            (prefix_arg : Value.t)
            ~raw_prefix_argument:
              (prefix_arg |> Raw_prefix_argument.of_value_exn : Raw_prefix_argument.t)]);
  [%expect
    {|
    ((prefix_arg          nil)
     (raw_prefix_argument Absent))
    ((prefix_arg 3) (raw_prefix_argument (Int 3)))
    ((prefix_arg (4)) (raw_prefix_argument (Nested 4)))
    ((prefix_arg          -)
     (raw_prefix_argument Minus))
    |}];
  return ()
;;

let give_emacs_chance_to_signal () =
  ignore (Value.Expert.process_input () : Value.Expert.Process_input.t);
  Value.Expert.raise_if_emacs_signaled ()
;;

let%expect_test "quit" =
  show_raise (fun () ->
    Command.Private.request_quit ();
    give_emacs_chance_to_signal ());
  [%expect {| (raised quit) |}];
  return ()
;;

let%expect_test "inhibit-quit" =
  show_raise (fun () ->
    Current_buffer.set_value_temporarily Sync inhibit_quit true ~f:(fun () ->
      Command.Private.request_quit ());
    give_emacs_chance_to_signal ());
  (* Once set_value_temporarily resets inhibit_quit back to nil, the quit gets raised. *)
  [%expect {| (raised quit) |}];
  show_raise (fun () -> give_emacs_chance_to_signal ());
  [%expect {| "did not raise" |}];
  show_raise (fun () ->
    Current_buffer.set_value_temporarily Sync inhibit_quit true ~f:(fun () ->
      Command.Private.request_quit ();
      give_emacs_chance_to_signal ();
      Command.Private.suppress_quit ()));
  [%expect {| "did not raise" |}];
  return ()
;;
