open! Core_kernel
open! Import

module Q = struct
  include Q

  let advice_add = "advice-add" |> Symbol.intern
  and advice_remove = "advice-remove" |> Symbol.intern
  and inner = "inner" |> Symbol.intern
  and rest = "rest" |> Symbol.intern
  ;;
end

module F = struct
  open! Funcall
  open! Value.Type

  let advice_add =
    Q.advice_add <: Symbol.type_ @-> Symbol.type_ @-> Symbol.type_ @-> return nil
  and advice_remove = Q.advice_remove <: Symbol.type_ @-> Symbol.type_ @-> return nil
  ;;
end

let add_predefined_function advice_name ~for_function =
  F.advice_add for_function Q.K.around advice_name
;;

let add_internal ?docstring ?interactive here advice_name f ~for_function =
  Defun.defun
    ?docstring
    ?interactive
    here
    Value.Type.value
    advice_name
    (let open Defun.Let_syntax in
     let%map_open () = return ()
     and inner = required Q.inner value
     and rest = rest Q.rest value in
     f inner rest);
  add_predefined_function advice_name ~for_function
;;

let around_values ?docstring ?interactive here advice_name f ~for_function =
  add_internal
    ?docstring
    ?interactive
    here
    advice_name
    (fun inner rest -> f (Value.funcallN inner) rest)
    ~for_function
;;

let around_funcall ?docstring ?interactive here advice_name funcall f ~for_function =
  add_internal
    ?docstring
    ?interactive
    here
    advice_name
    (Funcall.Private.advice funcall f)
    ~for_function
;;

let remove advice_name ~for_function = F.advice_remove for_function advice_name
