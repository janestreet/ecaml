open! Core_kernel
open! Import

let save_ save_function f =
  let r = ref None in
  let f =
    Function.create [%here] ~args:[]
      (function
        | [| |] -> r := Some (f ()); Value.nil
        | _ -> assert false)
  in
  ignore (
    Form.eval (
      Form.list
        [ save_function |> Form.symbol
        ; Form.list [ Q.funcall |> Form.symbol
                    ; f |> Function.to_value |> Form.quote ]])
    : Value.t);
  match !r with
  | None -> assert false
  | Some a -> a
;;

let save_current_buffer   f = save_ Q.save_current_buffer   f
let save_excursion        f = save_ Q.save_excursion        f
let save_restriction      f = save_ Q.save_restriction      f
let save_window_excursion f = save_ Q.save_window_excursion f
let save_selected_window  f = save_ Q.save_selected_window  f
