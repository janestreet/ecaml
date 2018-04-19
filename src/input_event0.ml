open! Core_kernel
open! Import

module Q = struct
  include Q
  let single_key_description           = "single-key-description"           |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "input-event"
    let here = [%here]
    let is_in_subtype = Value.is_event
  end)

let description t =
  Symbol.funcall1 Q.single_key_description (t |> to_value) |> Value.to_utf8_bytes_exn
;;

let sexp_of_t t = [%sexp (description t : string)]
