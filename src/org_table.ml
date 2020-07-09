open! Import

module Q = struct
  include Q

  let org_table = "org-table" |> Symbol.intern
  let orgtbl_mode = "orgtbl-mode" |> Symbol.intern
end

let feature = Q.org_table

let minor_mode =
  { Minor_mode.function_name = Q.orgtbl_mode; variable_name = Q.orgtbl_mode }
;;

let enable_minor_mode =
  let turn_on_orgtbl = Funcall.Wrap.("turn-on-orgtbl" <: nullary @-> return nil) in
  fun () ->
    Feature.require ("org" |> Symbol.intern);
    turn_on_orgtbl ()
;;
