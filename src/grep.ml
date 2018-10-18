open! Core_kernel
open! Import

module Q = struct
  include Q

  let ask = "ask" |> Symbol.intern
  and grep = "grep" |> Symbol.intern
  and grep_mode = "grep-mode" |> Symbol.intern
  and grep_save_buffers = "grep-save-buffers" |> Symbol.intern
  and grep_use_null_device = "grep-use-null-device" |> Symbol.intern
end

module Save_buffers = struct
  module T = struct
    type t =
      | Ask
      | False
      | True
    [@@deriving enumerate, sexp_of]
  end

  include T

  let ask = Q.ask |> Symbol.to_value

  let type_ =
    Value.Type.enum
      [%message "Grep.Save_buffers.t"]
      (module T)
      (function
        | Ask -> ask
        | False -> Value.nil
        | True -> Value.t)
  ;;
end

let save_buffers = Var.create Q.grep_save_buffers Save_buffers.type_
let use_null_device = Var.create Q.grep_use_null_device Value.Type.bool

let grep ~command =
  Current_buffer.set_value_temporarily
    (* Prevent [grep] from appending [/dev/null] to the command. *)
    use_null_device
    false
    ~f:(fun () -> Symbol.funcall1_i Q.grep (command |> Value.of_utf8_bytes))
;;

include (val Major_mode.wrap_existing [%here] Q.grep_mode)
