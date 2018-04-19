open! Core_kernel
open! Import

module Q = struct
  include Q
  let copy_marker                      = "copy-marker"                      |> Symbol.intern
  let make_marker                      = "make-marker"                      |> Symbol.intern
  let marker_buffer                    = "marker-buffer"                    |> Symbol.intern
  let marker_insertion_type            = "marker-insertion-type"            |> Symbol.intern
  let marker_position                  = "marker-position"                  |> Symbol.intern
  let set_marker_insertion_type        = "set-marker-insertion-type"        |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "marker"
    let here = [%here]
    let is_in_subtype = Value.is_marker
  end)

module Insertion_type = struct
  type t =
    | After_inserted_text
    | Before_inserted_text
  [@@deriving sexp_of]

  let of_value value =
    if value |> Value.to_bool
    then After_inserted_text
    else Before_inserted_text
  ;;

  let to_value = function
    | After_inserted_text  -> Value.t
    | Before_inserted_text -> Value.nil
  ;;
end

let buffer t =
  let value = Symbol.funcall1 Q.marker_buffer (t |> to_value) in
  if Value.is_nil value
  then None
  else Some (value |> Buffer.of_value_exn)
;;

let insertion_type t =
  Symbol.funcall1 Q.marker_insertion_type (t |> to_value)
  |> Insertion_type.of_value
;;

let set_insertion_type t insertion_type =
  Symbol.funcall2_i Q.set_marker_insertion_type
    (t |> to_value)
    (insertion_type |> Insertion_type.to_value)
;;

let position t =
  let value = Symbol.funcall1 Q.marker_position (t |> to_value) in
  if Value.is_nil value
  then None
  else Some (value |> Position.of_value_exn)
;;

let create () = Symbol.funcall0 Q.make_marker |> of_value_exn

let copy t = Symbol.funcall1 Q.copy_marker (t |> to_value) |> of_value_exn

let set t buffer position =
  Symbol.funcall3_i Q.set_marker
    (t |> to_value)
    (position |> Position.to_value)
    (buffer |> Buffer.to_value)
;;
