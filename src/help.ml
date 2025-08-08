open! Core
open! Import

let get_text_of_help ~invoke_help =
  Echo_area.inhibit_messages Sync invoke_help;
  let help_buf = Buffer.find ~name:"*Help*" |> Option.value_exn in
  Current_buffer.set_temporarily Sync help_buf ~f:(fun () ->
    Current_buffer.contents ()
    |> Text.to_utf8_bytes
    |> String.split_lines
    |> List.filter ~f:(fun line -> not (String.( = ) "[back]" (String.strip line)))
    |> concat ~sep:"\n"
    |> String.strip)
;;

let describe_function = Funcall.Wrap.("describe-function" <: Symbol.t @-> return nil)

let describe_function_text ?(obscure_symbol = false) symbol =
  let s = get_text_of_help ~invoke_help:(fun () -> describe_function symbol) in
  if obscure_symbol
  then
    String.Search_pattern.replace_all
      (String.Search_pattern.create (Symbol.name symbol))
      ~in_:s
      ~with_:"<SYMBOL>"
  else s
;;

let describe_key = Funcall.Wrap.("describe-key" <: Key_sequence.t @-> return nil)

let describe_key_text key_sequence =
  get_text_of_help ~invoke_help:(fun () -> describe_key key_sequence)
;;

let describe_minor_mode = Funcall.Wrap.("describe-minor-mode" <: Symbol.t @-> return nil)

let describe_minor_mode_text symbol =
  get_text_of_help ~invoke_help:(fun () -> describe_minor_mode symbol)
;;

let describe_variable = Funcall.Wrap.("describe-variable" <: Symbol.t @-> return nil)

let describe_variable_text symbol =
  get_text_of_help ~invoke_help:(fun () -> describe_variable symbol)
;;
