open! Core_kernel
open! Import

module Q = struct
  include Q
  let inhibit_message                  = "inhibit-message"                  |> Symbol.intern
  let message                          = "message"                          |> Symbol.intern
end

module Current_buffer = Current_buffer0

let message_value value = Symbol.funcall2_i Q.message (Value.of_utf8_bytes "%s") value

let message s = message_value (Value.of_utf8_bytes s)

let messagef fmt = ksprintf message fmt

let message_s sexp = messagef "%s" (sexp |> Sexp.to_string_hum)

let inhibit_message = Var.create Q.inhibit_message Value.Type.bool

let inhibit_messages f = Current_buffer.set_value_temporarily inhibit_message true ~f
