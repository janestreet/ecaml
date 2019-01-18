open! Core_kernel
open! Import0

module Q = struct
  let ns = "ns" |> Symbol.intern
  let pc = "pc" |> Symbol.intern
  let terminal_list = "terminal-list" |> Symbol.intern
  let terminal_live_p = "terminal-live-p" |> Symbol.intern
  let terminal_name = "terminal-name" |> Symbol.intern
  let w32 = "w32" |> Symbol.intern
  let x = "x" |> Symbol.intern
end

let is_terminal = Funcall.(Q.terminal_live_p <: value @-> return bool)

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
end

module F = struct
  open Funcall

  let terminal_list = Q.terminal_list <: nullary @-> return (list type_)
  let terminal_live_p = Q.terminal_live_p <: type_ @-> return Type.type_
  let terminal_name = Q.terminal_name <: type_ @-> return string
end

let all_live = F.terminal_list
let terminal_type = F.terminal_live_p
let name = F.terminal_name

let graphical t =
  match terminal_type t with
  | Text | Ms_dos -> false
  | X11 | Windows | Mac_os -> true
;;
