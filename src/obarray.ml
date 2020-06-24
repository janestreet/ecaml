open! Core_kernel
open! Import

include Value.Make_subtype (struct
    let name = "obarray"
    let here = [%here]
    let is_in_subtype = Value.is_vector
  end)

let standard = Current_buffer.value_exn Var.Wrap.("obarray" <: t)
let mapatoms = Funcall.Wrap.("mapatoms" <: Function.t @-> t @-> return nil)

let iter t ~f =
  let f =
    Defun.lambda
      [%here]
      (Returns Value.Type.unit)
      (let%map_open.Defun () = return ()
       and symbol = required "symbol" Symbol.t in
       f symbol)
  in
  mapatoms f t
;;
