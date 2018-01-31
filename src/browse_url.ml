open! Core_kernel
open! Import

module Url = String

module Q = struct
  let browse_url = "browse-url" |> Symbol.intern
end

let browse_url url = Symbol.funcall1_i Q.browse_url (url |> Value.of_utf8_bytes)
