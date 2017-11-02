open! Core_kernel
open! Import
open! Input_event

let examples =
  [ "C-c"
  ; "C-M-q"
  ; "<f5>"
  ; "C-<f5>"
  ; "C-<right>"
  ; "<mouse-2>"
  ; "C-<down-mouse-3>" ]
;;

let%expect_test "[create_exn]" =
  List.iter examples ~f:(fun example ->
    print_s [%message "" ~_:example ~_:(create_exn example : t)]);
  [%expect {|
    (C-c C-c)
    (C-M-q C-M-q)
    (<f5> <f5>)
    (C-<f5> <C-f5>)
    (C-<right> <C-right>)
    (<mouse-2> <mouse-2>)
    (C-<down-mouse-3> <C-down-mouse-3>) |}];
;;

let%expect_test "[create_exn] raise" =
  show_raise (fun () -> create_exn "abc");
  [%expect {|
    (raised (
      "[Input_event.create_exn] got key sequence not of length one"
      (input        abc)
      (key_sequence "a b c"))) |}];
;;

let%expect_test "[basic], [modifiers]" =
  List.iter examples ~f:(fun example ->
    let t = create_exn example in
    print_s [%message
      ""
        ~_:(example : string)
        ~basic:(basic t : Basic.t)
        ~modifiers:(modifiers t : Modifier.t list)]);
  [%expect {|
    (C-c (basic (Char_code 99)) (modifiers (Control)))
    (C-M-q
      (basic     (Char_code 113))
      (modifiers (Control   Meta)))
    (<f5> (basic (Symbol f5)) (modifiers ()))
    (C-<f5> (basic (Symbol f5)) (modifiers (Control)))
    (C-<right> (basic (Symbol right)) (modifiers (Control)))
    (<mouse-2> (basic (Symbol mouse-2)) (modifiers (Click)))
    (C-<down-mouse-3>
      (basic     (Symbol  mouse-3))
      (modifiers (Control Down))) |}];
;;

let%expect_test "[unread_command_input], [enqueue_unread_command_input]" =
  let show_unread () =
    print_s [%sexp (Current_buffer.value_exn unread_command_input : t list)] in
  show_unread ();
  [%expect {|
    () |}];
  enqueue_unread_command_input [];
  show_unread ();
  [%expect {|
    () |}];
  enqueue_unread_command_input [ create_exn "a" ];
  show_unread ();
  [%expect {|
    (a) |}];
  enqueue_unread_command_input [ create_exn "b"; create_exn "c" ];
  show_unread ();
  [%expect {|
    (a b c) |}];
  Current_buffer.set_value unread_command_input [];
  show_unread ();
  [%expect {|
    () |}];
;;

let%expect_test "[read]" =
  Key_sequence.enqueue_unread_command_input (Key_sequence.create_exn "a");
  print_s [%sexp (read () : t)];
  [%expect {| a |}];
;;
