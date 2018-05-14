open! Core_kernel
open! Import0

module Q = struct
  let require = "require" |> Symbol0.intern
end

let require t = Symbol0.funcall1_i (Q.require) (t |> Symbol0.to_value)
