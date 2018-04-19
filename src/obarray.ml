open! Core_kernel
open! Import

module Q = struct
  include Q
  let mapatoms                         = "mapatoms"                         |> Symbol.intern
  let obarray                          = "obarray"                          |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "obarray"
    let here = [%here]
    let is_in_subtype = Value.is_vector
  end)

let standard = Current_buffer.value_exn { symbol = Q.obarray; type_ }

let iter t ~f =
  let f =
    Function.create [%here]
      ~args:[ Q.symbol ]
      (function
        | [| symbol |] -> f (symbol |> Symbol.of_value_exn); Value.nil
        | _ -> assert false) in
  Symbol.funcall2_i Q.mapatoms (f |> Function.to_value) (t |> to_value)
;;
