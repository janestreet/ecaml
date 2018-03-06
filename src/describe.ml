open! Core_kernel
open! Import

module F = struct
  open! Funcall
  open! Value.Type

  let describe_function  =
    Symbol.intern "describe-function" <: Symbol.type_ @-> return nil
end

let function_ ?(obscure_symbol=false) symbol =
  Echo_area.inhibit_messages (fun () -> F.describe_function symbol);
  let s =
    Buffer.find ~name:"*Help*" |> Option.value_exn
    |> Current_buffer.set_temporarily ~f:(fun () ->
      Current_buffer.contents ()
      |> Text.to_utf8_bytes
      |> String.split_lines
      |> List.filter_map ~f:(fun line ->
        if ((am_running_inline_test && String.is_prefix line ~prefix:"Implemented at")
            || String.(=) "[back]" (String.strip line))
        then None
        else Some line)
      |> concat ~sep:"\n"
      |> String.strip)
  in
  if obscure_symbol
  then String.Search_pattern.replace_all
         (String.Search_pattern.create (Symbol.name symbol))
         ~in_:s
         ~with_:"<SYMBOL>"
  else s
;;
