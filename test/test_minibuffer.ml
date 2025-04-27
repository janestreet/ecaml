open! Core
open! Async_kernel
open! Import
open! Minibuffer

let%expect_test "[y_or_n]" =
  let test input =
    with_input_macro input (fun () ->
      let%bind bool = y_or_n ~prompt:"" in
      print_s [%sexp (bool : bool)];
      return ())
  in
  let%bind () = test "y RET" in
  [%expect {| true |}];
  let%bind () = test "n RET" in
  [%expect {| false |}];
  let%bind () = show_raise_async (fun () -> test "z RET") in
  [%expect
    {|
    ("[with_input_macro] provided full input but function is still running"
     (input "z RET") (minibuffer_prompt "Please answer y or n.  (y or n) ")
     (minibuffer_contents ""))
    (raised ("Ecaml_value__Value.Elisp_throw(_, _)"))
    |}];
  return ()
;;

let%expect_test "[y_or_n_with_timeout]" =
  let test input =
    with_input_macro input (fun () ->
      let%bind response =
        y_or_n_with_timeout ~prompt:"" ~timeout:(Time_ns.Span.microsecond, 13)
      in
      print_s [%sexp (response : int Y_or_n_with_timeout.t)];
      return ())
  in
  let%bind () = test "y RET" in
  [%expect {| Y |}];
  let%bind () = test "n RET" in
  [%expect {| N |}];
  let%bind () = show_raise_async (fun () -> test "z RET") in
  [%expect
    {|
    ("[with_input_macro] provided full input but function is still running"
     (input "z RET") (minibuffer_prompt "Please answer y or n.  (y or n) ")
     (minibuffer_contents ""))
    (raised ("Ecaml_value__Value.Elisp_throw(_, _)"))
    |}];
  return ()
;;

let%expect_test "[yes_or_no]" =
  let test input =
    with_input_macro input (fun () ->
      let%bind response = yes_or_no ~prompt:"" in
      print_s [%sexp (response : bool)];
      return ())
  in
  let%bind () = test "yes RET" in
  [%expect {| true |}];
  let%bind () = test "no RET" in
  [%expect {| false |}];
  return ()
;;

let%expect_test "[read_string]" =
  let read_from_insert_foo initial_contents =
    with_input_macro "foo RET" (fun () ->
      let%bind response =
        read_string ~initial_contents ~prompt_no_colon:"" ~history:Minibuffer.history ()
      in
      print_s [%sexp (response : string)];
      return ())
  in
  let%bind () = read_from_insert_foo Empty in
  [%expect {| foo |}];
  let%bind () = read_from_insert_foo (Point_at_end "contents") in
  [%expect {| contentsfoo |}];
  let%bind () = read_from_insert_foo (Point_at_pos ("contents", 0)) in
  [%expect {| foocontents |}];
  let%bind () = read_from_insert_foo (Point_at_pos ("contents", 5)) in
  [%expect {| contefoonts |}];
  return ()
;;

let%expect_test "[read_string] default value" =
  let read_string_with_default input =
    with_input_macro input (fun () ->
      let%bind response =
        read_string
          ~default_value:"this is the default"
          ~prompt_no_colon:""
          ~history:Minibuffer.history
          ()
      in
      print_s [%sexp (response : string)];
      return ())
  in
  let%bind () = read_string_with_default "RET" in
  [%expect {| "this is the default" |}];
  let%bind () = read_string_with_default "M-n RET" in
  [%expect {| "this is the default" |}];
  return ()
;;

let%expect_test "[exit_hook]" =
  print_s [%sexp (exit_hook : (_, _) Hook.t)];
  [%expect
    {|
    ((symbol    minibuffer-exit-hook)
     (hook_type Normal_hook)
     (value ((
       minibuffer--regexp-exit
       minibuffer-exit-on-screen-keyboard
       minibuffer-restore-windows))))
    |}];
  return ()
;;

let%expect_test "[setup_hook]" =
  print_s [%sexp (setup_hook : (_, _) Hook.t)];
  [%expect
    {|
    ((symbol    minibuffer-setup-hook)
     (hook_type Normal_hook)
     (value ((
       rfn-eshadow-setup-minibuffer
       minibuffer--regexp-setup
       minibuffer-setup-on-screen-keyboard
       minibuffer-error-initialize
       minibuffer-history-isearch-setup
       minibuffer-history-initialize))))
    |}];
  return ()
;;

let%expect_test "[setup_hook] [exit_hook]" =
  Hook.add
    setup_hook
    (Hook.Function.create
       ("test-setup" |> Symbol.intern)
       [%here]
       ~docstring:"<docstring>"
       ~hook_type:Normal_hook
       (Returns Value.Type.unit)
       (fun () ->
          print_s
            [%message
              "running setup hook"
                (Minibuffer.prompt () : string option)
                (Minibuffer.contents () : string)]));
  Hook.add
    exit_hook
    (Hook.Function.create
       ("test-exit" |> Symbol.intern)
       [%here]
       ~docstring:"<docstring>"
       ~hook_type:Normal_hook
       (Returns Value.Type.unit)
       (fun () ->
          print_s
            [%message
              "running exit hook"
                (Minibuffer.prompt () : string option)
                (Minibuffer.contents () : string)]));
  let%bind () =
    with_input_macro "foo RET" (fun () ->
      let%bind response =
        read_string ~prompt_no_colon:"prompt" ~history:Minibuffer.history ()
      in
      print_s [%sexp (response : string)];
      return ())
  in
  [%expect
    {|
    ("running setup hook"
      ("Minibuffer.prompt ()" ("prompt: "))
      ("Minibuffer.contents ()" ""))
    ("running exit hook"
      ("Minibuffer.prompt ()" ("prompt: "))
      ("Minibuffer.contents ()" foo))
    foo
    |}];
  return ()
;;
