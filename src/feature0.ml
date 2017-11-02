open! Core_kernel
open! Import

let require t = Symbol.funcall1_i Q.require (t |> Symbol.to_value)
