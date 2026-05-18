open! Core
open! Async_kernel
open! Import

let%expect_test "Emacs_backtrace.get" =
  (* Most of the frames have test directory strings as arguments; just grab the last two
     to prove this is a backtrace. *)
  List.take (Emacs_backtrace.get () |> String.split_lines |> List.rev) 2
  |> List.rev
  |> String.concat ~sep:"\n"
  |> print_endline;
  [%expect
    {|
    command-line()
    normal-top-level()
    |}];
  return ()
;;

module%test _ = struct
  (* Define a chain of Elisp functions: outer -> middle -> inner, where inner signals.
     Used by both backtrace tests below. *)
  let () =
    Form.Blocking.eval_i
      (Form.read
         {|
           (progn
             (defun ecaml-test-bt-inner ()
               (error "test-error-from-inner"))
             (defun ecaml-test-bt-middle ()
               (ecaml-test-bt-inner))
             (defun ecaml-test-bt-outer ()
               (ecaml-test-bt-middle)))
         |})
  ;;

  (* Censor the sandbox build directory prefix (which contains a non-deterministic hash)
     by replacing it with $BUILD_DIR/ in sexp output. *)
  let build_dir =
    let abs = Current_buffer.(get_buffer_local_exn directory) in
    let i = String.substr_index_exn abs ~pattern:"/app/emacs/" in
    String.prefix abs i ^ "/"
  ;;

  let sanitize_build_dir = replace_s ~pattern:build_dir ~with_:"$BUILD_DIR/"

  let call_outer =
    let f = Funcall.Wrap.("ecaml-test-bt-outer" <: nullary @-> return nil) in
    fun () ->
      require_does_raise ~sanitize:sanitize_build_dir (fun () -> f ());
      hide_positions_in_expect_test_output ()
  ;;

  let%expect_test "Elisp backtrace is NOT captured when debug-on-error is nil" =
    call_outer ();
    [%expect {| (test-error-from-inner) |}];
    return ()
  ;;

  let last_debugger_backtrace_and_signal =
    Var.Wrap.(
      "ecaml-test--last-debugger-backtrace-and-signal"
      <: nil_or (tuple3_as_list string value value))
  ;;

  let sanitize_debugger_backtrace bt =
    bt
    |> String.split_lines
    |> List.map ~f:(fun line ->
      Regexp.replace_string
        (Regexp.of_pattern "ecaml-func-[^(]*")
        ~with_:"ecaml-func-$POS"
        ~in_:line)
    |> List.map ~f:(fun line ->
      String.substr_replace_all line ~pattern:build_dir ~with_:"$BUILD_DIR/")
    |> String.concat ~sep:"\n"
  ;;

  let%expect_test "debugger fires when debug-on-error is t" =
    Current_buffer.set_value last_debugger_backtrace_and_signal None;
    Customization.set_value_temporarily Debugger.debug_on_error true ~f:(fun () ->
      call_outer ();
      [%expect {| (test-error-from-inner) |}];
      let bt = Current_buffer.value_exn last_debugger_backtrace_and_signal in
      (match bt with
       | None -> print_cr [%message "Expected debugger to fire and store backtrace"]
       | Some (bt, symbol, data) ->
         print_s [%message "debugger function fired" (symbol : Value.t) (data : Value.t)];
         print_endline "==============================";
         print_endline (sanitize_debugger_backtrace bt));
      [%expect
        {|
        ("debugger function fired"
          (symbol error)
          (data   test-error-from-inner))
        ==============================
          error("test-error-from-inner")
          ecaml-test-bt-inner()
          ecaml-test-bt-middle()
          ecaml-test-bt-outer()
          ecaml-func-$POS("./ecaml_test.cmxs")
          apply(ecaml-func-$POS
          ecaml-dynlink-loadfile("./ecaml_test.cmxs")
          eval((ecaml-dynlink-loadfile "./ecaml_test.cmxs") t)
          command-line-1(("--no-splash" "-l" "$BUILD_DIR/app/emacs/bin/emacs-inline-tests-runner.el" "--eval" "(ecaml-dynlink-loadfile \"./ecaml_test.cmxs\")" "--funcall" "ecaml--ppx-inline-tests-exit" "--"))
          command-line()
          normal-top-level()
        |}];
      return ())
  ;;
end
