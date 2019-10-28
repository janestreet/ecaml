open! Core_kernel
open! Import0

include Value.Make_subtype (struct
    let name = "buffer"
    let here = [%here]
    let is_in_subtype = Value.is_buffer
  end)

let equal = eq
let generate_new_buffer = Funcall.("generate-new-buffer" <: string @-> return t)
let create ~name = generate_new_buffer name

module Blocking = struct
  let kill = Funcall.("kill-buffer" <: t @-> return nil)
end

let kill t =
  Value.Private.run_outside_async [%here] ~allowed_in_background:true (fun () ->
    Blocking.kill t)
;;

let is_live t = Generated_bindings.buffer_live_p (t |> to_value)
