open! Core_kernel
open! Import

module Q = struct
  include Q
  let comment_end                      = "comment-end"                      |> Symbol.intern
  let comment_multi_line               = "comment-multi-line"               |> Symbol.intern
  let comment_start                    = "comment-start"                    |> Symbol.intern
end

let start = Var.create Q.comment_start Value.Type.string
let end_  = Var.create Q.comment_end   Value.Type.string

let multi_line = Var.create Q.comment_multi_line Value.Type.bool

let set_current_buffer_options ~start:s ~end_:e ~is_multi_line:m =
  Current_buffer.set_value start s;
  Current_buffer.set_value end_ e;
  Current_buffer.set_value multi_line m;
;;
