open! Core_kernel
open! Import0

module Q = struct
  let ns = "ns" |> Symbol.intern
  let pc = "pc" |> Symbol.intern
  let w32 = "w32" |> Symbol.intern
  let x = "x" |> Symbol.intern
end

let is_terminal = Funcall.Wrap.("terminal-live-p" <: value @-> return bool)

include Value.Make_subtype (struct
    let name = "terminal"
    let here = [%here]
    let is_in_subtype = is_terminal
  end)

module Type = struct
  module T = struct
    type t =
      | Text
      | X11
      | Windows
      | Mac_os
      | Ms_dos
    [@@deriving enumerate, sexp_of]
  end

  include T

  let type_ =
    Value.Type.enum
      [%message "terminal-type"]
      (module T)
      (function
        | Text -> Value.t
        | X11 -> Q.x |> Symbol.to_value
        | Windows -> Q.w32 |> Symbol.to_value
        | Mac_os -> Q.ns |> Symbol.to_value
        | Ms_dos -> Q.pc |> Symbol.to_value)
  ;;

  let t = type_
end

let all_live = Funcall.Wrap.("terminal-list" <: nullary @-> return (list t))
let terminal_type = Funcall.Wrap.("terminal-live-p" <: t @-> return Type.t)
let name = Funcall.Wrap.("terminal-name" <: t @-> return string)

let graphical t =
  match terminal_type t with
  | Text | Ms_dos -> false
  | X11 | Windows | Mac_os -> true
;;
