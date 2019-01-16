open! Core_kernel
open! Import

module Q = struct
  include Q

  let advice_add = "advice-add" |> Symbol.intern
  and advice_remove = "advice-remove" |> Symbol.intern
  and inner = "inner" |> Symbol.intern
  and rest = "rest" |> Symbol.intern
end

module F = struct
  open! Funcall
  open! Value.Type

  let advice_add =
    Q.advice_add <: Symbol.type_ @-> Symbol.type_ @-> Symbol.type_ @-> return nil
  and advice_remove = Q.advice_remove <: Symbol.type_ @-> Symbol.type_ @-> return nil
end

let add_predefined_function advice_name ~for_function =
  F.advice_add for_function Q.K.around advice_name
;;

let add_internal advice_name here ~for_function ?docstring ?interactive f =
  Defun.defun
    advice_name
    here
    ?docstring
    ?interactive
    (Returns Value.Type.value)
    (let open Defun.Let_syntax in
     let%map_open () = return ()
     and inner = required Q.inner value
     and rest = rest Q.rest value in
     f inner rest);
  add_predefined_function advice_name ~for_function
;;

let around_values advice_name here ?docstring ~for_function ?interactive f =
  add_internal advice_name here ?docstring ~for_function ?interactive (fun inner rest ->
    f (Value.funcallN inner) rest)
;;

let around_funcall advice_name here ?docstring ~for_function ?interactive funcall f =
  add_internal
    advice_name
    here
    ~for_function
    ?docstring
    ?interactive
    (Funcall.Private.advice funcall f)
;;

let remove advice_name ~for_function = F.advice_remove for_function advice_name
