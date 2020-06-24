open! Core_kernel
open! Import

module Format = struct
  include Value.Make_subtype (struct
      let name = "mode-line-format"
      let here = [%here]
      let is_in_subtype _ = true
    end)

  let in_buffer = Buffer_local.Wrap.("mode-line-format" <: t)
end

let text = Funcall.Wrap.("format-mode-line" <: Format.t @-> return Text.t)
