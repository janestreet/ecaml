open! Import

module Q = struct
  include Q

  let org_table = "org-table" |> Symbol.intern
  and orgtbl_mode = "orgtbl-mode" |> Symbol.intern
  and turn_on_orgtbl = "turn-on-orgtbl" |> Symbol.intern
end

let feature = Q.org_table

let minor_mode =
  { Minor_mode.function_name = Q.orgtbl_mode; variable_name = Q.orgtbl_mode }
;;

let enable_minor_mode () = Symbol.funcall0_i Q.turn_on_orgtbl
