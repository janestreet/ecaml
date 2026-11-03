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
    ~initialize:(fun () -> print_s [%message "initialized"])
    ()
;;

let major_mode = define_mode test_mode [%here]

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
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    show_major_mode ();
    [%expect
      {|
      ((wrapped_at app/emacs/lib/ecaml/src/major_mode.ml:LINE:COL)
       (symbol fundamental-mode)
       (keymap_var (fundamental-mode-map keymap))
       (hook (
         Error
         "fundamental-mode has no mode hook. [(Info-goto-node \"(elisp) Major Modes\")]")))
      |}];
    change_to major_mode ~in_:(Current_buffer.get ());
    [%expect {| initialized |}];
    show_major_mode ();
    [%expect
      {|
      ((wrapped_at app/emacs/lib/ecaml/test/test_major_mode.ml:LINE:COL)
       (symbol test-major-mode)
       (keymap_var (test-major-mode-map keymap))
       (hook (
         Ok (
           (symbol    test-major-mode-hook)
           (hook_type Normal_hook)
           (value (()))))))
      |}]);
  return ()
;;

let%expect_test "[hook]" =
  let t = define_mode ("for-testing-mode-hook" |> Symbol.intern) [%here] in
  Hook.add
    (hook t |> ok_exn)
    (Hook.Function.create
       ("my-hook" |> Symbol.intern)
       [%here]
       ~docstring:"<docstring>"
       ~hook_type:Normal_hook
       (Returns Value.Type.unit)
       (fun () -> print_s [%message "hook ran"]));
  let buffer = Buffer.create ~name:"for-testing-mode-hook" in
  change_to t ~in_:buffer;
  [%expect
    {|
    initialized
    "hook ran"
    |}];
  let%bind () = Buffer.kill buffer in
  return ()
;;

let%expect_test "[is_derived]" =
  print_s [%sexp (is_derived text ~from:special : bool)];
  [%expect {| false |}];
  let major_mode =
    define_derived_mode
      ("texting-mode" |> Symbol.intern)
      [%here]
      ~docstring:"<docstring>"
      ~mode_line:"texting"
      ~parent:text
      ()
  in
  print_s [%sexp (is_derived major_mode ~from:text : bool)];
  [%expect {| true |}];
  return ()
;;
