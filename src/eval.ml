open! Core_kernel
open! Import

module Q = struct
  let eval_after_load = "eval-after-load" |> Symbol.intern
end

let after_load here feature ~f =
  let fn =
    Function.create here ~args:[] (fun _ ->
      f ();
      Value.nil)
  in
  Form.(
    Blocking.eval_i
      (list
         [ symbol Q.eval_after_load
         ; quote (feature |> Symbol.to_value)
         ; quote (fn |> Function.to_value)
         ]))
;;
