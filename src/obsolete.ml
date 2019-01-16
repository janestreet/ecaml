open! Core_kernel
open! Import

module Q = struct
  let make_obsolete = "make-obsolete" |> Symbol.intern
  and make_obsolete_variable = "make-obsolete-variable" |> Symbol.intern
end

module F = struct
  open Funcall
  open Value.Type

  let make_obsolete =
    Q.make_obsolete <: Symbol.type_ @-> Symbol.type_ @-> string @-> return nil
  ;;

  let make_obsolete_variable =
    Q.make_obsolete_variable <: Symbol.type_ @-> Symbol.type_ @-> string @-> return nil
  ;;
end

let make_function_obsolete obsolete ~current ~since =
  F.make_obsolete obsolete current since
;;

let make_variable_obsolete obsolete ~current ~since =
  F.make_obsolete_variable obsolete current since
;;
