open! Core_kernel
open! Async_kernel
open! Import
open! Key_sequence

let%expect_test "[create_exn]" =
  List.iter
    [ ""
    ; "C-c y"
    ; "C-M-q"
    ; "<f5>"
    ; "C-<f5>"
    ; "C-<right>"
    ; "<mouse-2>"
    ; "C-<down-mouse-3>"
    ]
    ~f:(fun string ->
      print_s [%message "" ~_:(string : string) ~_:(create_exn string : t)]);
  [%expect
    {|
    ("" "")
    ("C-c y" "C-c y")
    (C-M-q C-M-q)
    (<f5> <f5>)
    (C-<f5> <C-f5>)
    (C-<right> <C-right>)
    (<mouse-2> <mouse-2>)
    (C-<down-mouse-3> <C-down-mouse-3>) |}];
  return ()
;;

let%expect_test "[create_exn] raise" =
  show_raise (fun () -> create_exn "C-xy");
  [%expect {| (raised ("C- must prefix a single character, not xy")) |}];
  return ()
;;

let%expect_test "[length]" =
  List.iter [ ""; "C-x"; "a"; "ab"; "A-C-M-S-x" ] ~f:(fun string ->
    print_s [%message "" ~_:string ~_:(length (create_exn string) : int)]);
  [%expect {|
    ("" 0)
    (C-x 1)
    (a 1)
    (ab 2)
    (A-C-M-S-x 1) |}];
  return ()
;;

let%expect_test "[get]" =
  let t = create_exn "abc" in
  for i = 0 to length t - 1 do
    print_s [%sexp (get t i : Input_event.t)]
  done;
  [%expect {|
    a
    b
    c |}];
  return ()
;;

let%expect_test "[execute]" =
  Current_buffer.set_temporarily_to_temp_buffer Async (fun () ->
    let%bind () = execute (create_exn "foo RET bar RET") in
    print_s [%sexp (Current_buffer.contents () : Text.t)];
    [%expect {| "foo\nbar\n" |}];
    return ())
;;

let print_am_executing () =
  print_s [%sexp (Current_buffer.value_exn am_executing : bool)]
;;

let%expect_test "[am_executing] false" =
  print_am_executing ();
  [%expect {| false |}];
  return ()
;;

let%expect_test "[am_executing] true" =
  defun_nullary_nil ("foo" |> Symbol.intern) [%here] ~interactive:No_arg (fun () ->
    print_am_executing ());
  let%bind () = execute (create_exn "M-x foo") in
  [%expect {| true |}];
  return ()
;;

let%expect_test "[enqueue_unread_command_input]" =
  let show () =
    print_s
      [%sexp
        (Current_buffer.value_exn Input_event.unread_command_input : Input_event.t list)]
  in
  show ();
  [%expect {| () |}];
  enqueue_unread_command_input (create_exn "abc");
  show ();
  [%expect {| (a b c) |}];
  Current_buffer.set_value Input_event.unread_command_input [];
  return ()
;;

let%expect_test "[read]" =
  enqueue_unread_command_input (create_exn "a");
  print_s [%sexp (read () ~prompt:"" : t)];
  [%expect {| a |}];
  enqueue_unread_command_input (create_exn "C-x C-f");
  print_s [%sexp (read () ~prompt:"" : t)];
  [%expect {| "C-x C-f" |}];
  return ()
;;

let%expect_test "[to_list]" =
  List.iter [ ""; "a"; "RET"; "C-c C-a" ] ~f:(fun s ->
    print_s
      [%message
        "" ~_:(s : string) ~_:(s |> create_exn |> to_list : Input_event.t list)]);
  [%expect {|
    ("" ())
    (a (a))
    (RET (RET))
    ("C-c C-a" (C-c C-a)) |}];
  return ()
;;
