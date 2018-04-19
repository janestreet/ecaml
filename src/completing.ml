open! Core_kernel
open! Import

module Q = struct
  include Q
  let completing_read                  = "completing-read"                  |> Symbol.intern
  let completing_read_multiple         = "completing-read-multiple"         |> Symbol.intern
end

module Initial_input = struct
  type t =
    | Empty
    | Point_at_end of string
    | Point_at_pos of string * int

  let to_value = function
    | Empty -> Symbol.to_value Q.nil
    | Point_at_end s -> Value.of_utf8_bytes s
    | Point_at_pos (s, i) -> Value.cons (Value.of_utf8_bytes s) (Value.of_int_exn i)
  ;;

end

module Require_match = struct
  type t =
    | Confirm
    | Confirm_after_completion
    | False
    | Require_match_or_null
    | True

  let to_value t =
    let symbol =
      match t with
      | Confirm -> Symbol.intern "confirm"
      | Confirm_after_completion -> Symbol.intern "confirm-after-completion"
      | False -> Q.nil
      | Require_match_or_null -> Symbol.intern "other"
      | True -> Q.t
    in
    Symbol.to_value symbol
  ;;

end

let read
      ?default
      ?history
      ?(initial_input=Initial_input.Empty)
      ?(require_match=Require_match.False)
      ()
      ~collection
      ~prompt
  =
  let predicate = Value.nil in
  let prompt =
    match default with
    | None -> prompt
    | Some d -> concat [ prompt; "(default = "; d; ") " ] in
  Symbol.funcallN Q.completing_read
    [ prompt |> Value.of_utf8_bytes
    ; collection |> List.map ~f:Value.of_utf8_bytes |> Value.list
    ; predicate
    ; require_match |> Require_match.to_value
    ; initial_input |> Initial_input.to_value
    ; history |> Value.option Symbol.to_value
    ; default |> Value.option Value.of_utf8_bytes
    ]
  |> Value.to_utf8_bytes_exn
;;

let crm_separator = Var.create ("crm-separator" |> Symbol.intern) Value.Type.string

let read_multiple
      ?default
      ?history
      ?(initial_input=Initial_input.Empty)
      ?(require_match=Require_match.False)
      ?(separator_regexp="[ \t]*,[ \t]*")
      ()
      ~collection
      ~prompt
  =
  let predicate = Value.nil in
  Current_buffer.set_value_temporarily crm_separator separator_regexp ~f:(fun () ->
    Symbol.funcallN Q.completing_read_multiple
      [ prompt |> Value.of_utf8_bytes
      ; collection |> Value.Type.(list string).to_value
      ; predicate
      ; require_match |> Require_match.to_value
      ; initial_input |> Initial_input.to_value
      ; history |> Value.option Symbol.to_value
      ; default |> Value.option Value.of_utf8_bytes
      ]
    |> Value.to_list_exn ~f:Value.to_utf8_bytes_exn)
;;
