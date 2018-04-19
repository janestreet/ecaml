open! Core_kernel
open! Import

module Q = struct
  include Q
  let advice_add                       = "advice-add"                       |> Symbol.intern
  let advice_remove                    = "advice-remove"                    |> Symbol.intern
  let inner                            = "inner"                            |> Symbol.intern
  let rest                             = "rest"                             |> Symbol.intern
end

module F = struct
  open! Funcall
  open! Value.Type

  let advice_add    =
    Q.advice_add    <: Symbol.type_ @-> Symbol.type_ @-> Symbol.type_ @-> return nil
  let advice_remove =
    Q.advice_remove <: Symbol.type_ @-> Symbol.type_ @-> return nil
end

let add_predefined_function advice_name ~for_function =
  F.advice_add for_function Q.K.around advice_name
;;

let add ?docstring ?interactive here type_ advice_name f ~for_function =
  Defun.defun ?docstring ?interactive here type_ advice_name
    (let open Defun.Let_syntax in
     let%map_open () = return ()
     and inner = required Q.inner Value.Type.value
     and rest  = rest     Q.rest  Value.Type.value
     in
     f ~inner:(Value.funcallN inner) ~inner_args:rest);
  add_predefined_function advice_name ~for_function;
;;

let remove advice_name ~for_function = F.advice_remove for_function advice_name
