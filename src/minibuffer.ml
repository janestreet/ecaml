open! Core_kernel
open! Import

let read_from
      ?default_value
      ?(history_list = Q.minibuffer_history)
      ?history_list_pos
      ?initial_contents
      ()
      ~prompt
  =
  Symbol.funcallN Q.read_from_minibuffer
    [ prompt |> Value.of_utf8_bytes
    ; (match initial_contents with
       | None -> Value.nil
       | Some s -> (s |> Value.of_utf8_bytes))
    ; Value.nil
    ; Value.nil
    ; (match history_list_pos with
       | None -> history_list |> Symbol.to_value
       | Some i -> Value.cons (history_list |> Symbol.to_value) (i |> Value.of_int_exn))
    ; (match default_value with
       | None -> Value.nil
       | Some s -> s |> Value.of_utf8_bytes) ]
  |> Value.to_utf8_bytes_exn
;;
