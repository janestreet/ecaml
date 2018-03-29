open! Core_kernel
open! Import

module Q = struct
  let inhibit_redisplay = "inhibit-redisplay" |> Symbol.intern
  let redisplay         = "redisplay"         |> Symbol.intern
end

let redisplay ?(force = false) () =
  Symbol.funcall1_i Q.redisplay (force |> Value.of_bool);
;;

let inhibit_redisplay = Var.create Q.inhibit_redisplay Value.Type.bool
