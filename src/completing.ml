open! Core_kernel
open! Import

module Q = struct
  include Q

  let completing_read = "completing-read" |> Symbol.intern
  and completing_read_multiple = "completing-read-multiple" |> Symbol.intern
  and confirm = "confirm" |> Symbol.intern
  and confirm_after_completion = "confirm-after-completion" |> Symbol.intern
end

module Initial_input = struct
  type t =
    | Empty
    | Point_at_end of string
    | Point_at_pos of string * int
  [@@deriving sexp_of]

  let to_value = function
    | Empty -> Symbol.to_value Q.nil
    | Point_at_end s -> Value.of_utf8_bytes s
    | Point_at_pos (s, i) -> Value.cons (Value.of_utf8_bytes s) (Value.of_int_exn i)
  ;;

  let of_value_exn value =
    List.find_map_exn
      ~f:(fun f -> f value)
      [ (fun value ->
          match Symbol.equal (Symbol.of_value_exn value) Q.nil with
          | true -> Some Empty
          | false -> None
          | exception _ -> None)
      ; (fun value ->
           match Value.to_utf8_bytes_exn value with
           | s -> Some (Point_at_end s)
           | exception _ -> None)
      ; (fun value ->
           match Value.Type.(tuple string int).of_value_exn value with
           | s, i -> Some (Point_at_pos (s, i))
           | exception _ -> None)
      ]
  ;;

  let type_ =
    Value.Type.create
      [%sexp "completing", "initial-input"]
      [%sexp_of: t]
      of_value_exn
      to_value
  ;;
end

module Require_match = struct
  type t =
    | Confirm
    | Confirm_after_completion
    | False
    | Require_match_or_null
    | True

  let ({ Value.Type.of_value_exn; to_value; _ } as type_) =
    Value.Type.map
      Symbol.type_
      ~name:[%sexp "completing", "require-match"]
      ~of_:(fun symbol ->
        List.Assoc.find
          [ Q.confirm, Confirm
          ; Q.confirm_after_completion, Confirm_after_completion
          ; Q.nil, False
          ; Q.t, True
          ]
          symbol
          ~equal:Symbol.equal
        |> Option.value ~default:Require_match_or_null)
      ~to_:(function
        | Confirm -> Q.confirm
        | Confirm_after_completion -> Q.confirm_after_completion
        | False -> Q.nil
        | Require_match_or_null -> Symbol.intern "other"
        | True -> Q.t)
  ;;

  let default = False
end

module Collection = (val Ocaml_or_elisp_value.make Value.Type.(list string_cached))

module Blocking = struct
  let read
        ?default
        ?(history : _ Var.t option)
        ?(initial_input = Initial_input.Empty)
        ?(require_match = Require_match.default)
        ()
        ~collection
        ~prompt
    =
    let predicate = Value.nil in
    let prompt =
      match default with
      | None -> prompt
      | Some d -> concat [ prompt; "(default = "; d; ") " ]
    in
    Symbol.funcallN
      Q.completing_read
      [ prompt |> Value.of_utf8_bytes
      ; collection |> Collection.to_value
      ; predicate
      ; require_match |> Require_match.to_value
      ; initial_input |> Initial_input.to_value
      ; (match history with
         | None -> Value.nil
         | Some history -> history.symbol |> Symbol.to_value)
      ; default |> Value.option Value.of_utf8_bytes
      ]
    |> Value.to_utf8_bytes_exn
  ;;

  let crm_separator = Var.create ("crm-separator" |> Symbol.intern) Value.Type.string

  let read_multiple
        ?default
        ?(history : _ Var.t option)
        ?(initial_input = Initial_input.Empty)
        ?(require_match = Require_match.False)
        ?(separator_regexp = "[ \t]*,[ \t]*")
        ()
        ~collection
        ~prompt
    =
    let predicate = Value.nil in
    Current_buffer.set_value_temporarily crm_separator separator_regexp ~f:(fun () ->
      Symbol.funcallN
        Q.completing_read_multiple
        [ prompt |> Value.of_utf8_bytes
        ; collection |> Collection.to_value
        ; predicate
        ; require_match |> Require_match.to_value
        ; initial_input |> Initial_input.to_value
        ; (match history with
           | None -> Value.nil
           | Some history -> history.symbol |> Symbol.to_value)
        ; default |> Value.option Value.of_utf8_bytes
        ]
      |> Value.to_list_exn ~f:Value.to_utf8_bytes_exn)
  ;;
end

let read ?default ?history ?initial_input ?require_match () ~collection ~prompt =
  Async_ecaml.Private.run_outside_async (fun () ->
    Blocking.read
      ?default
      ?history
      ?initial_input
      ?require_match
      ()
      ~collection
      ~prompt)
;;

let read_multiple
      ?default
      ?history
      ?initial_input
      ?require_match
      ?separator_regexp
      ()
      ~collection
      ~prompt
  =
  Async_ecaml.Private.run_outside_async (fun () ->
    Blocking.read_multiple
      ?default
      ?history
      ?initial_input
      ?require_match
      ?separator_regexp
      ()
      ~collection
      ~prompt)
;;
