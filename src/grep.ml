open! Core_kernel
open! Import

module Q = struct
  include Q
  let grep                             = "grep"                             |> Symbol.intern
  let grep_use_null_device             = "grep-use-null-device"             |> Symbol.intern
end

let use_null_device = Var.create Q.grep_use_null_device Value.Type.bool

let grep ~command =
  Current_buffer.set_value_temporarily
    (* Prevent [grep] from appending [/dev/null] to the command. *)
    use_null_device false
    ~f:(fun () -> Symbol.funcall1_i Q.grep (command |> Value.of_utf8_bytes))
;;
