open! Core
open! Async_kernel
module Time_ns = Time_ns_unix
include Expect_test_helpers_core
include Expect_test_helpers_async
include Ecaml
include File_path.Operators

let concat = String.concat

(* [ignore_stderr] is useful for tests where Emacs outputs on stderr, which would cause
   jenga to view the test as failing. [ignore_stderr] redirects stderr to [/dev/null], and
   returns a function to restore stderr. *)
let ignore_stderr () =
  let module Unix = Core_unix in
  let saved_stderr = Unix.dup Unix.stderr in
  Unix.dup2 ~src:(Unix.openfile "/dev/null" ~mode:[ O_WRONLY ]) ~dst:Unix.stderr ();
  stage (fun () ->
    Unix.dup2 ~src:saved_stderr ~dst:Unix.stderr ();
    Unix.close saved_stderr)
;;

let ignoring_stderr f =
  let restore = unstage (ignore_stderr ()) in
  protect ~f ~finally:restore
;;

let background color : Text.Face_spec.t = [ Attributes [ T (Background, Color color) ] ]
let foreground color : Text.Face_spec.t = [ Attributes [ T (Foreground, Color color) ] ]
let background_blue = background Color.blue
let background_red = background Color.red
let foreground_red = foreground Color.red
let foreground_blue = foreground Color.blue

let show_last_match ?subexp () =
  let module Last_match = Regexp.Last_match in
  print_s
    [%message
      ""
        ~text:
          (Or_error.try_with (fun () -> Last_match.text_exn ?subexp ())
           : Text.t Or_error.t)
        ~start:
          (Or_error.try_with (fun () -> Last_match.start_exn ?subexp ()) : int Or_error.t)
        ~end_:
          (Or_error.try_with (fun () -> Last_match.end_exn ?subexp ()) : int Or_error.t)]
;;

let print_s ?(hide_positions = false) ?(templatize_current_directory = false) sexp =
  if not templatize_current_directory
  then print_s sexp ~hide_positions
  else
    print_endline
      (sexp
       |> Sexp_pretty.sexp_to_string
       |> (if hide_positions
           then Expect_test_helpers_core.hide_positions_in_string
           else Fn.id)
       |> fun string ->
       String.Search_pattern.replace_all
         (String.Search_pattern.create
            (File.truename Current_buffer.(get_buffer_local_exn directory)))
         ~in_:string
         ~with_:"<current-directory>/")
;;

let touch filename =
  ignore
    (Process.call_exn "touch" [ filename ] ~working_directory:Of_current_buffer : string)
;;

let int_var name = Var.create (Symbol.create_uninterned ~name) Value.Type.int

let show_current_buffer_local_variables () =
  print_s
    [%sexp
      (Buffer.buffer_local_variables (Current_buffer.get ())
       |> List.sort ~compare:(fun (s1, _) (s2, _) -> Symbol.compare_name s1 s2)
       : (Symbol.t * _ option) list)]
;;

(* [with_input_macro keystrokes f] simulates typing [keystrokes] into [f] by defining a
   keyboard macro that invokes [f] and then replays [keystrokes].

   It then sends some additional keystrokes that invoke a command to assert that [f] is
   not still running (to avoid timing out reading from a minibuffer, in tests). *)

let with_input_macro =
  let with_input_macro_map =
    Keymap.defvar
      ("ecaml-with-input-macro-map" |> Symbol.intern)
      ~docstring:"with_input_macro transient map"
  in
  let start_sequence = "C-c e" in
  let raise_if_still_running_sequence = "C-H-M-e" in
  let f_is_running = ref false in
  let string_ref = ref "<should never be seen>" in
  let f_ref = ref (fun () -> return ()) in
  Defun.defun_nullary
    ("ecaml-input-macro-start" |> Symbol.intern)
    [%here]
    ~define_keys:[ with_input_macro_map, start_sequence ]
    ~docstring:"Start with_input_macro input."
    ~interactive:No_arg
    (Returns_deferred Value.Type.unit)
    (fun () ->
      Monitor.protect
        (fun () ->
          f_is_running := true;
          !f_ref ())
        ~finally:(fun () ->
          f_is_running := false;
          return ()));
  let raise_if_running = "ecaml-input-macro-raise-if-running" |> Symbol.intern in
  let top_level = Funcall.Wrap.("top-level" <: nullary @-> return ignored) in
  Defun.defun_nullary
    raise_if_running
    [%here]
    ~docstring:"Raise at the end of with_input_macro if it's still running"
    ~interactive:No_arg
    (Returns Value.Type.unit)
    (fun () ->
       match !f_is_running with
       | false -> ()
       | true ->
         let minibuffer_prompt = Minibuffer.prompt () in
         let minibuffer_contents =
           if Minibuffer.depth () > 0 then Some (Minibuffer.contents ()) else None
         in
         (* If the [f] is stuck inside [read-from-minibuffer], any exceptions raised here
            will be caught and not propagated past [read-from-minibuffer]. Instead,
            message and call [top-level] (which does a [throw] instead of [signal]). *)
         message_s
           [%message
             "[with_input_macro] provided full input but function is still running"
               ~input:(!string_ref : string)
               (minibuffer_prompt : (string option[@sexp.option]))
               (minibuffer_contents : (string option[@sexp.option]))];
         top_level ());
  Keymap.global_set raise_if_still_running_sequence (Symbol raise_if_running);
  fun string f ->
    let keyseq =
      String.concat ~sep:" " [ start_sequence; string; raise_if_still_running_sequence ]
      |> Key_sequence.create_exn
    in
    f_is_running := false;
    string_ref := string;
    f_ref := f;
    Keymap.set_transient (Current_buffer.value_exn with_input_macro_map);
    Key_sequence.execute keyseq
;;

let%expect_test "[with_input_macro] raises if [f] is still running after receiving \
                 keystrokes"
  =
  let test () =
    with_input_macro "zzz" (fun () ->
      let%bind input =
        Minibuffer.read_string ~prompt_no_colon:"input" ~history:Minibuffer.history ()
      in
      (* This test is specifically trying to exercise the condition that the full input,
         so don't let the test "pass" the [require_does_raise_async] by raising here. *)
      print_cr [%message "Should not reach this point" (input : string)];
      return ())
  in
  let%bind () = require_does_raise_async test in
  [%expect
    {|
    ("[with_input_macro] provided full input but function is still running"
     (input zzz) (minibuffer_prompt "input: ") (minibuffer_contents zzz))
    ("Ecaml_value__Value.Elisp_throw(_, _)")
    |}];
  return ()
;;

let () = Customization.set_value Backup.make_backup_files false

let while_ predicate ~do_ =
  Deferred.repeat_until_finished () (fun () ->
    if not (predicate ())
    then return (`Finished ())
    else (
      let%bind () = do_ () in
      return (`Repeat ())))
;;
