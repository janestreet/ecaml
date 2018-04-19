open! Core_kernel
open! Import

module Q = struct
  include Q
  let characterp                       = "characterp"                       |> Symbol.intern
  let max_char                         = "max-char"                         |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "char-code"
    let here = [%here]
    let is_in_subtype value = Symbol.funcall1 Q.characterp value |> Value.to_bool
  end)

let equal t1 t2 = eq t1 t2

let of_int_exn int = int |> Value.of_int_exn |> of_value_exn

let to_int t = t |> to_value |> Value.to_int_exn

let min_value = of_int_exn 0

let max_value = Symbol.funcall0 Q.max_char |> of_value_exn

let of_char_exn char =
  let code = char |> Char.to_int in
  if code >= 128
  then raise_s [%message
         "[Char_code.of_char_exn] got non-ASCII character" (char : char) (code : int)];
  code |> Value.of_int_exn |> of_value_exn
;;
