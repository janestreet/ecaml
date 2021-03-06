open! Core
open! Import

let[@cold] funcall0 f =
  lambda_nullary_nil [%here] (fun () -> f ())
  |> Function.to_value
  |> (Value.funcall0 [@inlined never])
  |> Value.Type.(unit |> of_value_exn)
;;

let filename = __FILE__
