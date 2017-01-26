open! Core
open! Async.Std
open! Import

let message_t =
  let message = Symbol.intern "message" in
  fun t ->
    Symbol.funcall_i message [ Value.of_string "%s"; t ]
;;

let message s = message_t (Value.of_string s)

let messagef fmt = ksprintf message fmt
