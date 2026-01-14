(** Functions used only in tests.

    Note that this is included in the main ecaml_plugin.so for uniformity, so don't do
    anything at top-level which shouldn't run in production. *)

open! Core
open! Async
open! Ecaml
open! Expect_test_helpers_core
module Buffer_helper = Buffer_helper
module Env = Env

module Q = struct
  let ecaml_test_error_in_minibuffer = "ecaml-test-error-in-minibuffer" |> Symbol.intern
end

let show ?(show_point = true) () =
  if show_point
  then Buffer_helper.show_point ()
  else Current_buffer.contents () |> Text.to_utf8_bytes |> message
;;

let raise_if_in_minibuffer_key = "<f13>"
let minibuffer_was_open = ref false

let is_minibuffer_open =
  Funcall.Wrap.("ecaml-test-minibuffer-open-p" <: nullary @-> return bool)
;;

let () =
  defun_nullary_nil
    (* In emacs-inline-tests-runner.el, this command is bound to
       raise_if_in_minibuffer_key globally and in query-replace-map. *)
    ("ecaml-test-raise-if-in-minibuffer" |> Symbol.intern)
    [%here]
    ~docstring:"For testing."
    ~interactive:No_arg
    (fun () ->
       match is_minibuffer_open () with
       | false -> minibuffer_was_open := false
       | true ->
         minibuffer_was_open := true;
         let prompt = Minibuffer.prompt () in
         message_s [%sexp "Minibuffer open", { prompt : string option }];
         never_returns (Command.abort_recursive_edit ()))
;;

let execute_keys keys =
  let current_buffer = Current_buffer.get () in
  let selected_buffer = Window.buffer_exn (Selected_window.get ()) in
  if not (Buffer.equal current_buffer selected_buffer)
  then
    raise_s
      [%message
        "do not execute a key sequence when the current_buffer and selected_buffer are \
         different -- [execute-kbd-macro] sets the current_buffer to the selected_buffer"
          (current_buffer : Buffer.t)
          (selected_buffer : Buffer.t)];
  Key_sequence.execute (Key_sequence.create_exn (concat keys ~sep:" "))
;;

let press ?(and_show = true) ?show_point key_sequence =
  minibuffer_was_open := false;
  let%map () = execute_keys [ key_sequence; raise_if_in_minibuffer_key ] in
  if and_show && not !minibuffer_was_open then show ?show_point ()
;;

let show_minibuffer_key = "<f14>"

let show_minibuffer_key_sequence ~show_contents =
  let prefix =
    match show_contents with
    | true -> "C-u "
    | false -> ""
  in
  [%string {|%{prefix}%{show_minibuffer_key}|}]
;;

let () =
  let show_minibuffer ~show_contents =
    let prompt =
      Minibuffer.prompt ()
      |> Option.value_exn ~message:"no prompt even though minibuffer active"
    in
    let contents =
      match show_contents with
      | false -> ""
      | true -> Minibuffer.contents ()
    in
    print_endline [%string {|%{prompt}%{contents}|}]
  in
  let show_completions () =
    let name = "*Completions*" in
    Option.iter (Buffer.find ~name) ~f:(fun buffer ->
      print_endline "";
      print_endline [%string {|Contents of %{name} buffer:|}];
      Current_buffer.set_temporarily Sync buffer ~f:(fun () ->
        print_endline (Current_buffer.contents () |> Text.to_utf8_bytes)))
  in
  defun
    (* In emacs-inline-tests-runner.el, this command is bound to show_minibuffer_key
       globally. *)
    ("ecaml-test-show-minibuffer" |> Symbol.intern)
    [%here]
    ~docstring:"For testing."
    ~interactive:Raw_prefix
    (Returns Value.Type.unit)
    (let%map_open.Defun show_contents = required "SHOW-CONTENTS" bool in
     match Minibuffer.active_window () with
     | None ->
       print_endline "Minibuffer not open";
       require false
     | Some _ ->
       show_minibuffer ~show_contents;
       show_completions ())
;;

let ecaml_test_passthrough_command_error_function =
  let throw = Funcall.Wrap.("throw" <: Symbol.t @-> value @-> return ignored) in
  Defun.defun_func
    ("ecaml-test-passthrough-command-error-function" |> Symbol.intern)
    [%here]
    ~docstring:"For testing."
    (Returns Value.Type.unit)
    (let%map_open.Defun () = return ()
     and error = required "error" value
     and _context = required "context" value
     and _signal = required "signal" value in
     throw Q.ecaml_test_error_in_minibuffer error)
;;

(* Normally, [read-from-minibuffer] doesn't pass command errors through to the caller, but
   such errors still abort keyboard macro execution. This means that the call to
   [execute-kbd-macro] can hang indefinitely without returning/raising if it triggers any
   error while in a minibuffer prompt. See [minibuffer-error-initialize] which arranges
   for this behavior in the minibuffer.

   We override this by appending to [minibuffer-setup-hook] and using a different
   [command-error-function] which simply [throw]s the exception. We don't want to use
   [command-error-default-function] here, because in batch mode it kills the Emacs process
   directly (exit code 255). *)
let raise_command_errors_through_minibuffer =
  let command_error_function = Var.Wrap.("command-error-function" <: Function.t) in
  let error_message_string =
    Funcall.Wrap.("error-message-string" <: value @-> return string)
  in
  let hook_function =
    Hook.Function.create
      ("ecaml-test-minibuffer-error-override" |> Symbol.intern)
      [%here]
      ~docstring:"For testing."
      ~hook_type:Normal_hook
      (Returns Value.Type.unit)
      (fun () ->
         Current_buffer.make_buffer_local command_error_function;
         Current_buffer.set_value
           command_error_function
           ecaml_test_passthrough_command_error_function)
  in
  fun f ->
    (* Add to the global [minibuffer-setup-hook] because we want to affect all minibuffer
       invocations during [f], not just the first one. *)
    Hook.add
      (Minibuffer.setup_hook [@alert "-prefer_with_setup_hook_instead"])
      ~where:End
      hook_function;
    let%map result =
      Monitor.try_with ~extract_exn:true (fun () ->
        (* Disable backtraces they contain nondeterministic things like memory addresses
           of bytecode functions. This only affects errors that we deliberately cause to
           exit uncleanly out of [execute-kbd-macro], namely, the [throw] in
           [ecaml-test-passthrough-command-error-function]. *)
        Current_buffer.set_value_temporarily
          Async
          Var.Wrap.("backtrace-on-error-noninteractive" <: bool)
          false
          ~f)
    in
    Hook.remove
      (Minibuffer.setup_hook [@alert "-prefer_with_setup_hook_instead"])
      hook_function;
    match result with
    | Error (Value.For_testing.Elisp_throw { tag; value } as exn)
      when Value.eq tag (Symbol.to_value Q.ecaml_test_error_in_minibuffer) ->
      message_s [%message "Command errored in minibuffer" ~_:(error_message_string value)];
      (* Reraise to prevent [press_and_show_minibuffer] from printing a CR. *)
      raise exn
    | Error exn -> raise exn
    | Ok () -> ()
;;

let completions_format = Customization.Wrap.("completions-format" <: Symbol.t)

let press_and_show_minibuffer ?(show_contents = true) key_sequence =
  (* Format completions in one column rather than attempting to group them in columns.
     This is a little easier to read in test diffs, and also avoids the *Completions*
     buffer including TAB characters when you get unlucky. *)
  Customization.set_value completions_format (Symbol.intern "one-column");
  let%bind () =
    (* Ensure we do not display a stale completions buffer, only one that was created
       during the execution of [key_sequence]. *)
    match Buffer.find ~name:"*Completions*" with
    | None -> return ()
    | Some buffer -> Buffer.kill buffer
  in
  match%map
    try_with (fun () ->
      raise_command_errors_through_minibuffer (fun () ->
        execute_keys [ key_sequence; show_minibuffer_key_sequence ~show_contents; "C-]" ]))
  with
  | Error _ -> ()
  | Ok () -> require false
;;

let eval string =
  let%map value = Form.eval_string string in
  print_s [%sexp (value : Value.t)]
;;

let tmpdir_var = "TMPDIR"

let set_tmpdir_temporarily tmpdir ~f =
  let tmpdir = Filename.to_directory tmpdir in
  let old_tmpdir = Env.getenv tmpdir_var in
  let old_temp_file_directory =
    Customization.value Directory.for_temp_files_customization
  in
  Monitor.protect
    (fun () ->
      Env.putenv ~key:tmpdir_var ~data:tmpdir;
      Customization.set_value Directory.for_temp_files_customization tmpdir;
      f ())
    ~finally:(fun () ->
      (match old_tmpdir with
       | None -> Env.unsetenv tmpdir_var
       | Some old_tmpdir -> Env.putenv ~key:tmpdir_var ~data:old_tmpdir);
      Customization.set_value
        Directory.for_temp_files_customization
        old_temp_file_directory;
      return ())
;;
