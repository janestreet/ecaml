open! Core
open! Async
open! Import

module Q = struct
  module K = struct
    let emergency = ":emergency" |> Symbol.intern
    let error = ":error" |> Symbol.intern
    let warning = ":warning" |> Symbol.intern
    let debug = ":debug" |> Symbol.intern
  end
end

module Level = struct
  type t =
    | Emergency
    | Error
    | Warning
    | Debug

  let to_symbol : t -> Symbol.t = function
    | Emergency -> Q.K.emergency
    | Error -> Q.K.error
    | Warning -> Q.K.warning
    | Debug -> Q.K.debug
  ;;
end

let display_warning =
  Funcall.Wrap.(
    "display-warning" <: list Symbol.t @-> Text.t @-> Symbol.t @-> return ignored)
;;

let display message ~type_ ~level =
  display_warning
    (Nonempty_list.to_list type_)
    (Text.of_utf8_bytes message)
    (Level.to_symbol level)
;;

let display_text message ~type_ ~level =
  display_warning (Nonempty_list.to_list type_) message (Level.to_symbol level)
;;
