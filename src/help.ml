open! Core
open! Import

let get_text_of_help ~invoke_help =
  Echo_area.inhibit_messages Sync invoke_help;
  let help_buf = Buffer.find ~name:"*Help*" |> Option.value_exn in
  Current_buffer.set_temporarily Sync help_buf ~f:(fun () ->
    Current_buffer.contents ()
    |> Text.to_utf8_bytes
    |> String.split_lines
    |> List.filter_map ~f:(fun line ->
      if (Ppx_inline_test_lib.am_running
          && String.is_prefix line ~prefix:"Implemented at")
      || String.( = ) "[back]" (String.strip line)
      then None
      else Some line)
    |> concat ~sep:"\n"
    |> String.strip)
;;

let describe_function = Funcall.Wrap.("describe-function" <: Symbol.t @-> return nil)

let sort_keybindings text =
  let is_start_of_keybindings =
    let regexp =
      Regexp.of_rx
        Rx.(
          Seq
            [ Line Start
            ; Exactly "key"
            ; One_or_more (Exactly " ")
            ; Exactly "binding"
            ; Line End
            ])
    in
    fun line -> Regexp.does_match regexp (Text.of_utf8_bytes line)
  in
  let lines = String.split_lines text in
  match List.findi lines ~f:(fun _ -> is_start_of_keybindings) with
  | None -> text
  | Some (index, _) ->
    let mode_description, key_bindings = List.split_n lines (index + 2) in
    String.concat
      ~sep:"\n"
      (List.concat
         [ mode_description
         ; key_bindings
           |> List.filter ~f:(not << String.is_empty)
           |> List.sort ~compare:String.compare
         ])
;;

let describe_function_text ?(obscure_symbol = false) symbol =
  let s =
    get_text_of_help ~invoke_help:(fun () -> describe_function symbol) |> sort_keybindings
  in
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

let where_is_internal =
  Funcall.Wrap.(
    "where-is-internal"
    <: Symbol.t @-> nil_or Keymap.t @-> bool @-> return (nil_or Key_sequence.t))
;;

let where_is symbol = where_is_internal symbol None true

let where_is_string symbol =
  Option.value_map
    (where_is symbol)
    ~f:Key_sequence.description
    ~default:(Symbol.name symbol)
;;
