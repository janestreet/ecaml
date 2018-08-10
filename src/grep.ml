open! Core_kernel
open! Import

module Q = struct
  include Q

  let ask = "ask" |> Symbol.intern
  and grep = "grep" |> Symbol.intern
  and grep_save_buffers = "grep-save-buffers" |> Symbol.intern
  and grep_use_null_device = "grep-use-null-device" |> Symbol.intern
  ;;
end

module Save_buffers = struct
  type t =
    | Ask
    | False
    | True
  [@@deriving sexp_of]

  let ask = Q.ask |> Symbol.to_value

  let to_value = function
    | Ask -> ask
    | False -> Value.nil
    | True -> Value.t
  ;;

  let of_value_exn value =
    if Value.is_nil value
    then False
    else if Value.equal value ask
    then Ask
    else if Value.is_function value
    then
      raise_s
        [%message
          "[Grep.Save_buffers.of_value_exn] doesn't support function values" [%here]]
    else True
  ;;

  let type_ =
    Value.Type.create
      [%message "Grep.Save_buffers.t"]
      [%sexp_of: t]
      of_value_exn
      to_value
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
