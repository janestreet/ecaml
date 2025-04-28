open! Core
open! Async
open! Ecaml
open! Expect_test_helpers_core
module Buffer_helper = Buffer_helper

module Q = struct
  let format_message = "format-message" |> Symbol.intern

  let jane_fe_test_passthrough_command_error_function =
    "jane-fe-test-passthrough-command-error-function" |> Symbol.intern
  ;;

  let jane_fe_test_error_in_minibuffer =
    "jane-fe-test-error-in-minibuffer" |> Symbol.intern
  ;;
end

let () =
  (* [minibuffer-message] works by placing an overlay with property [after-string] in the
     minibuffer, and then deleting it after [sit-for].  To show such messages in tests,
     just advise [minibuffer-message].

     We don't need to do something similar for [set-minibuffer-message], which is
     potentially called by normal [message] but only in interactive Emacs. *)
  Advice.add
    ~to_function:(Symbol.intern "minibuffer-message")
    (Advice.defun_around_values
       ("jane-fe-test-report-minibuffer-message" |> Symbol.intern)
       ~docstring:"For testing."
       Sync
       (fun f args ->
          let formatted_msg =
            Value.funcallN (Q.format_message |> Symbol.to_value) args
            |> Value.to_utf8_bytes_exn
          in
          messagef "[minibuffer-message] %s" formatted_msg;
          f args))
;;

let show ?(show_point = true) () =
  if show_point
  then Buffer_helper.show_point ()
  else Current_buffer.contents () |> Text.to_utf8_bytes |> message
;;

let raise_if_in_minibuffer_key = "<f13>"
let minibuffer_was_open = ref false
let read_event_depth = ref 0

let () =
  let advice =
    Advice.defun_around_values
      ("jane-fe-test-count-read-event-depth" |> Symbol.intern)
      ~docstring:"For testing."
      Sync
      (fun f args ->
         incr read_event_depth;
         Exn.protect ~f:(fun () -> f args) ~finally:(fun () -> decr read_event_depth))
  in
  let commands_that_call_read_event_manually =
    (* Add advice around any functions that explicitly call [read-event] or
       [read-char{,-exclusive}].  These functions don't make [query-replace-map] active, and
       messages aren't shown in the echo area ([current-message] returns nil) in
       noninteractive emacs, so it's hard to otherwise determine if we're in key-by-key
       prompting function. *)
    [ "map-y-or-n-p" ]
  in
  List.iter commands_that_call_read_event_manually ~f:(fun name ->
    Advice.add advice ~to_function:(Symbol.intern name))
;;

let is_minibuffer_open () =
  match Minibuffer.active_window () with
  | Some _ -> true
  | None -> !read_event_depth > 0
;;

let () =
  let query_replace_map =
    Var.Wrap.("query-replace-map" <: Keymap.t) |> Current_buffer.value_exn
  in
  defun_nullary_nil
    ("jane-fe-test-raise-if-in-minibuffer" |> Symbol.intern)
    [%here]
    ~docstring:"For testing."
    ~interactive:No_arg
    ~define_keys:
      [ Keymap.global (), raise_if_in_minibuffer_key
      ; query_replace_map, raise_if_in_minibuffer_key
      ]
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
    ("jane-fe-test-show-minibuffer" |> Symbol.intern)
    [%here]
    ~docstring:"For testing."
    ~interactive:Raw_prefix
    ~define_keys:[ Keymap.global (), show_minibuffer_key ]
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

let () =
  let throw = Funcall.Wrap.("throw" <: Symbol.t @-> value @-> return ignored) in
  defun
    Q.jane_fe_test_passthrough_command_error_function
    [%here]
    ~docstring:"For testing."
    (Returns Value.Type.unit)
    (let%map_open.Defun () = return ()
     and error = required "error" value
     and _context = required "context" value
     and _signal = required "signal" value in
     throw Q.jane_fe_test_error_in_minibuffer error)
;;

(* Normally, [read-from-minibuffer] doesn't pass command errors through to the caller, but
   such errors still abort keyboard macro execution.  This means that the call to
   [execute-kbd-macro] can hang indefinitely without returning/raising if it triggers any
   error while in a minibuffer prompt.  See [minibuffer-error-initialize] which arranges
   for this behavior in the minibuffer.

   We override this by appending to [minibuffer-setup-hook] and using a different
   [command-error-function] which simply [throw]s the exception.  We don't want to use
   [command-error-default-function] here, because in batch mode it kills the Emacs process
   directly (exit code 255). *)
let raise_command_errors_through_minibuffer =
  let command_error_function = Var.Wrap.("command-error-function" <: Function.t) in
  let error_message_string =
    Funcall.Wrap.("error-message-string" <: value @-> return string)
  in
  let hook_function =
    Hook.Function.create
      ("jane-fe-test-minibuffer-error-override" |> Symbol.intern)
      [%here]
      ~docstring:"For testing."
      ~hook_type:Normal_hook
      (Returns Value.Type.unit)
      (fun () ->
         Current_buffer.make_buffer_local command_error_function;
         Current_buffer.set_value
           command_error_function
           (Function.of_symbol Q.jane_fe_test_passthrough_command_error_function))
  in
  fun f ->
    Hook.add Minibuffer.setup_hook ~where:End hook_function;
    let%map result =
      Monitor.try_with ~extract_exn:true (fun () ->
        (* Disable backtraces they contain nondeterministic things like memory addresses
           of bytecode functions.  This only affects errors that we deliberately cause to
           exit uncleanly out of [execute-kbd-macro], namely, the [throw] in
           [jane-fe-test-passthrough-command-error-function]. *)
        Current_buffer.set_value_temporarily
          Async
          Var.Wrap.("backtrace-on-error-noninteractive" <: bool)
          false
          ~f)
    in
    Hook.remove Minibuffer.setup_hook hook_function;
    match result with
    | Error (Value.For_testing.Elisp_throw { tag; value } as exn)
      when Value.eq tag (Symbol.to_value Q.jane_fe_test_error_in_minibuffer) ->
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
