open! Core_kernel
open! Import

let defvar here symbol initial_value ~docstring =
  ignore
    ( Form.eval
        ([ Q.defvar |> Symbol.to_value
         ; symbol |> Symbol.to_value
         ; Value.list [ Symbol.to_value Q.quote; initial_value ]
         ; docstring |> Value.of_utf8_bytes
         ]
         |> Value.list
         |> Form.of_value_exn)
      : Value.t );
  Load_history.add_entry here (Var symbol)
;;
