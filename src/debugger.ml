open! Core_kernel
open! Import

let debug_on_error = Customization.Wrap.("debug-on-error" <: bool)
let toggle_debug_on_error_symbol = "toggle-debug-on-error" |> Symbol.intern
