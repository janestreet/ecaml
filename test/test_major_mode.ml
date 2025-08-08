open! Core
open! Async_kernel
open! Import
open! Major_mode

let test_mode = "test-major-mode" |> Symbol.intern

let define_mode sym here =
  define_derived_mode
    sym
    here
    ~docstring:"docstring"
    ~mode_line:"<test-mode mode line>"
    ~initialize:(Returns Value.Type.unit, fun () -> print_s [%message "initialized"])
    ()
;;

module M = (val define_mode test_mode [%here])

let%expect_test "duplicate [define_derived_mode]" =
  show_raise ~hide_positions:true (fun () -> define_mode test_mode [%here]);
  [%expect
    {|
    (raised (
      "Already associated with a name."
      (name test-major-mode)
      (wrapped_at app/emacs/lib/ecaml/test/test_major_mode.ml:LINE:COL)
      (previous_def (
        (wrapped_at app/emacs/lib/ecaml/test/test_major_mode.ml:LINE:COL)
        (symbol test-major-mode)
        (keymap_var (test-major-mode-map keymap))
        (name <opaque>)
        (hook (
          Ok (
            (symbol    test-major-mode-hook)
            (hook_type Normal_hook)
            (value (())))))))))
    |}];
  return ()
;;

let%expect_test "duplicate name is NOT caught" =
  show_raise (fun () -> define_mode ("other-mode" |> Symbol.intern) [%here]);
  [%expect {| "did not raise" |}];
  return ()
;;

let%expect_test "[define_derived_mode]" =
  let show_major_mode () =
    print_s [%sexp (Current_buffer.major_mode () : t)] ~hide_positions:true
  in
  Current_buffer.set_temporarily_to_temp_buffer Async (fun () ->
    show_major_mode ();
    [%expect
      {|
      ((wrapped_at app/emacs/lib/ecaml/src/major_mode.ml:LINE:COL)
       (symbol fundamental-mode)
       (keymap_var (fundamental-mode-map keymap))
       (name <opaque>)
       (hook (
         Error
         "fundamental-mode has no mode hook. [(Info-goto-node \"(elisp) Major Modes\")]")))
      |}];
    let%bind () = Current_buffer.change_major_mode M.major_mode in
    [%expect {| initialized |}];
    show_major_mode ();
    [%expect
      {|
      ((wrapped_at app/emacs/lib/ecaml/test/test_major_mode.ml:LINE:COL)
       (symbol test-major-mode)
       (keymap_var (test-major-mode-map keymap))
       (name <opaque>)
       (hook (
         Ok (
           (symbol    test-major-mode-hook)
           (hook_type Normal_hook)
           (value (()))))))
      |}];
    return ())
;;

let%expect_test "[hook]" =
  let module M = (val define_mode ("for-testing-mode-hook" |> Symbol.intern) [%here]) in
  let t = M.major_mode in
  Hook.add
    (hook t |> ok_exn)
    (Hook.Function.create
       ("my-hook" |> Symbol.intern)
       [%here]
       ~docstring:"<docstring>"
       ~hook_type:Normal_hook
       (Returns Value.Type.unit)
       (fun () -> print_s [%message "hook ran"]));
  let%bind () =
    Current_buffer.set_temporarily_to_temp_buffer Async (fun () ->
      Current_buffer.change_major_mode t)
  in
  [%expect
    {|
    initialized
    "hook ran"
    |}];
  return ()
;;

let%expect_test "[is_derived]" =
  print_s [%sexp (is_derived Text.major_mode ~from:Special.major_mode : bool)];
  [%expect {| false |}];
  let module M =
    (val define_derived_mode
           ("texting-mode" |> Symbol.intern)
           [%here]
           ~docstring:"<docstring>"
           ~mode_line:"texting"
           ~parent:Text.major_mode
           ())
  in
  print_s [%sexp (is_derived M.major_mode ~from:Text.major_mode : bool)];
  [%expect {| true |}];
  return ()
;;

let%expect_test "Async initialization function" =
  let module M =
    (val define_derived_mode
           ("test-major-mode-async-init-function" |> Symbol.intern)
           [%here]
           ~docstring:"docstring"
           ~mode_line:"<test-mode mode line>"
           ~initialize:
             ( Returns_deferred Value.Type.unit
             , fun () ->
                 let%map () = Clock_ns.after (Time_ns.Span.of_ms 1.) in
                 print_s [%message "initialized"] )
           ())
  in
  let%bind () = Current_buffer.change_major_mode M.major_mode in
  [%expect {| initialized |}];
  return ()
;;
