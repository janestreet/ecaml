open! Core_kernel
open! Import

module Q = struct
  include Q

  let inhibit_message = "inhibit-message" |> Symbol.intern
end

module Current_buffer = Current_buffer0

let message = Value.message
let messagef = Value.messagef
let message_s = Value.message_s
let inhibit_message = Var.create Q.inhibit_message Value.Type.bool
let inhibit_messages f = Current_buffer.set_value_temporarily inhibit_message true ~f
