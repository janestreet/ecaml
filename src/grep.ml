open! Core_kernel
open! Import

module Q = struct
  include Q

  let ask = "ask" |> Symbol.intern
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

  let t = type_
end

let save_buffers = Customization.Wrap.("grep-save-buffers" <: Save_buffers.t)
let use_null_device = Customization.Wrap.("grep-use-null-device" <: bool)
let grep = Funcall.Wrap.("grep" <: string @-> return nil)

let grep ~command =
  (* Prevent [grep] from appending [/dev/null] to the command. *)
  Current_buffer.set_value_temporarily
    Sync
    (use_null_device |> Customization.var)
    false
    ~f:(fun () -> grep command)
;;

include (val Major_mode.wrap_existing "grep-mode" [%here])
