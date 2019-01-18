open! Core_kernel
open! Import

let debug_on_error = Var.create ("debug-on-error" |> Symbol.intern) Value.Type.bool
