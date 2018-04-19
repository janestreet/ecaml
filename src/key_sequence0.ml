open! Core_kernel
open! Import

module Q = struct
  include Q
  let elt                  = "elt"                  |> Symbol.intern
  let key_description      = "key-description"      |> Symbol.intern
  let listify_key_sequence = "listify-key-sequence" |> Symbol.intern
  let read_kbd_macro       = "read-kbd-macro"       |> Symbol.intern
end

module Z = struct
  module Input_event = Input_event0
end

open Z

include Value.Make_subtype (struct
    let name = "key-sequence"
    let here = [%here]
    let is_in_subtype t = Value.is_string t || Value.is_vector t
  end)

let description t =
  Symbol.funcall1 Q.key_description (t |> to_value) |> Value.to_utf8_bytes_exn
;;

let sexp_of_t t = [%sexp (description t : string)]

let create_exn string =
  Symbol.funcall1 Q.read_kbd_macro (string |> Value.of_utf8_bytes)
  |> of_value_exn
;;

let length t = Symbol.funcall1 Q.length (t |> to_value) |> Value.to_int_exn

let get t i =
  Symbol.funcall2 Q.elt (t |> to_value) (i |> Value.of_int_exn)
  |> Input_event.of_value_exn
;;

let to_list t =
  Symbol.funcall1 Q.listify_key_sequence (t |> to_value)
  |> Value.to_list_exn ~f:Input_event.of_value_exn
;;
