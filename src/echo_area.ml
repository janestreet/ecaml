open! Core_kernel
open! Import

let message_value value = Symbol.funcall2_i Q.message (Value.of_utf8_bytes "%s") value

let message s = message_value (Value.of_utf8_bytes s)

let messagef fmt = ksprintf message fmt

let message_s sexp = messagef "%s" (sexp |> Sexp.to_string_hum)

let inhibit_messages f =
  Symbol.set_value_temporarily Q.inhibit_message Value.t ~f
;;
