open! Core_kernel
open! Import

let start = Var.Wrap.("comment-start" <: string)
let end_ = Var.Wrap.("comment-end" <: string)
let multi_line = Var.Wrap.("comment-multi-line" <: bool)

let set_current_buffer_options ~start:s ~end_:e ~is_multi_line:m =
  Current_buffer.set_value start s;
  Current_buffer.set_value end_ e;
  Current_buffer.set_value multi_line m
;;
