open! Core_kernel
open! Import

module Q = struct
  let require = "require" |> Symbol.intern
end

let require t = Symbol.funcall1_i Q.require (t |> Symbol.to_value)
