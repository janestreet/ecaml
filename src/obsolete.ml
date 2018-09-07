open! Core_kernel
open! Import

module Q = struct
  let define_obsolete_function_alias = "define-obsolete-function-alias" |> Symbol.intern
  and make_obsolete = "make-obsolete" |> Symbol.intern
end

module F = struct
  open Funcall
  open Value.Type

  let make_obsolete =
    Q.make_obsolete <: Symbol.type_ @-> Symbol.type_ @-> option string @-> return nil
  ;;
end

let alias ?docstring ?when_ obsolete ~current =
  let open Form in
  eval_i
    (list
       [ symbol Q.define_obsolete_function_alias
       ; quote (Symbol.to_value obsolete)
       ; quote (Symbol.to_value current)
       ; Option.map when_ ~f:string |> Option.value ~default:nil
       ; Option.map docstring ~f:string |> Option.value ~default:nil
       ])
;;

let make ?when_ obsolete ~current = F.make_obsolete obsolete current when_
