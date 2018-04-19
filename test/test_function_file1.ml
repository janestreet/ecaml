open! Core_kernel
open! Import

let [@inline never] funcall0 f =
  Function.create [%here] ~args:[] (fun (_ : Value.t array) ->
    f ();
    Value.nil)
  |> Function.to_value
  |> Value.funcall0
  |> Value.Type.unit.of_value_exn
;;

let filename = __FILE__
