open! Core_kernel
open! Import0

module Q = struct
  include Q

  let generate_new_buffer = "generate-new-buffer" |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "buffer"
    let here = [%here]
    let is_in_subtype = Value.is_buffer
  end)

let equal = eq

let create ~name =
  Symbol.funcall1 Q.generate_new_buffer (name |> Value.of_utf8_bytes) |> of_value_exn
;;

let kill t = Symbol.funcall1_i Q.kill_buffer (t |> to_value)
