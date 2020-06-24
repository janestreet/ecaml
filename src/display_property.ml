open! Core_kernel
open! Import0

module Q = struct
  include Q

  let margin = "margin" |> Symbol.intern
  let left_margin = "left-margin" |> Symbol.intern
  let right_margin = "right-margin" |> Symbol.intern
end

module Margin = struct
  type t =
    | Left
    | Right
  [@@deriving sexp_of]

  let to_value = function
    | Left -> Q.left_margin |> Symbol.to_value
    | Right -> Q.right_margin |> Symbol.to_value
  ;;
end

type t = Display_in_margin of Margin.t [@@deriving sexp_of]

let to_values (t : t) =
  match t with
  | Display_in_margin left_or_right ->
    [ Q.margin |> Symbol.to_value; Margin.to_value left_or_right ]
;;

let of_values_exn (car, cdr) =
  match
    [ Symbol.of_value_exn car |> Symbol.name; Symbol.of_value_exn cdr |> Symbol.name ]
  with
  | [ "margin"; "left-margin" ] -> Display_in_margin Left
  | [ "margin"; "right-margin" ] -> Display_in_margin Right
  | _ -> raise_s [%message "Unexpected value" (car : Value.t) (cdr : Value.t)]
;;
