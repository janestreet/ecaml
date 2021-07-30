open! Core
open! Async
open! Ecaml
open! Expect_test_helpers_core
module Buffer_helper = Buffer_helper

let show ?(show_point = true) () =
  if show_point
  then Buffer_helper.show_point ()
  else Current_buffer.contents () |> Text.to_utf8_bytes |> message
;;

let raise_if_in_minibuffer_key = "<f13>"
let minibuffer_was_open = ref false

let () =
  defun_nullary_nil
    ("jane-fe-test-raise-if-in-minibuffer" |> Symbol.intern)
    [%here]
    ~docstring:"For testing."
    ~interactive:No_arg
    ~define_keys:[ Keymap.global (), raise_if_in_minibuffer_key ]
    (fun () ->
       match Minibuffer.active_window () with
       | None -> minibuffer_was_open := false
       | Some _ ->
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
       require [%here] false
     | Some _ ->
       show_minibuffer ~show_contents;
       show_completions ())
;;

let press_and_show_minibuffer ?(show_contents = true) key_sequence =
  match%map
    try_with
      ~run:
        `Schedule
      ~rest:`Log
      (fun () ->
         execute_keys [ key_sequence; show_minibuffer_key_sequence ~show_contents; "C-g" ])
  with
  | Error _ -> ()
  | Ok () -> require [%here] false
;;

let eval string =
  let%map value = Form.eval_string string in
  print_s [%sexp (value : Value.t)]
;;
