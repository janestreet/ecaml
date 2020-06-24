open! Core_kernel
open! Async_kernel
open! Import

module Q = struct
  include Q

  let confirm = "confirm" |> Symbol.intern
  let confirm_after_completion = "confirm-after-completion" |> Symbol.intern
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
           match Value.Type.(tuple string int |> of_value_exn) value with
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

  let t = type_
end

module Require_match = struct
  type t =
    | Confirm
    | Confirm_after_completion
    | False
    | Require_match_or_null
    | True

  let type_ =
    Value.Type.map
      Symbol.t
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

  let t = type_
  let of_value_exn = Value.Type.of_value_exn type_
  let to_value = Value.Type.to_value type_
  let default = False
end

module Collection = (val Ocaml_or_elisp_value.make Value.Type.(list string_cached))

let completing_read =
  let completing_read =
    Funcall.Wrap.(
      "completing-read"
      <: string
         @-> value
         @-> value
         @-> Require_match.t
         @-> Initial_input.t
         @-> Symbol.t
         @-> nil_or string
         @-> return string)
  in
  fun ~prompt
    ~collection
    ?(predicate = Value.nil)
    ?(require_match = Require_match.default)
    ?(initial_input = Initial_input.Empty)
    ?default
    ~history
    () ->
    Async_ecaml.Private.run_outside_async [%here] (fun () ->
      let prompt =
        match default with
        | None -> prompt
        | Some d -> concat [ prompt; "(default = "; d; ") " ]
      in
      completing_read
        prompt
        collection
        predicate
        require_match
        initial_input
        (Minibuffer.History.symbol history)
        default)
;;

let read ~prompt ~collection ?require_match ?initial_input ?default ~history () =
  completing_read
    ~prompt
    ~collection:(collection |> Collection.to_value)
    ?require_match
    ?initial_input
    ?default
    ~history
    ()
;;

let read_map_key ~prompt ~collection ?initial_input ?default ~history () =
  let%bind choice =
    read
      ~prompt
      ~collection:
        (This (Map.keys collection))
      ~require_match:True
      ?initial_input
      ?default
      ~history
      ()
  in
  return (Map.find_exn collection choice)
;;

let crm_separator = Var.Wrap.("crm-separator" <: string)

let read_multiple =
  let completing_read_multiple =
    Funcall.Wrap.(
      "completing-read-multiple"
      <: string
         @-> value
         @-> value
         @-> Require_match.t
         @-> Initial_input.t
         @-> Symbol.t
         @-> nil_or string
         @-> return (list string))
  in
  fun ~prompt
    ~collection
    ?(require_match = Require_match.False)
    ?(separator_regexp = "[ \t]*,[ \t]*")
    ?(initial_input = Initial_input.Empty)
    ?default
    ~history
    () ->
    Async_ecaml.Private.run_outside_async [%here] (fun () ->
      let predicate = Value.nil in
      let prompt =
        match default with
        | None -> prompt
        | Some d -> concat [ prompt; "(default = "; d; ") " ]
      in
      Current_buffer.set_value_temporarily
        Sync
        crm_separator
        separator_regexp
        ~f:(fun () ->
          completing_read_multiple
            prompt
            (collection |> Collection.to_value)
            predicate
            require_match
            initial_input
            (Minibuffer.History.symbol history)
            default))
;;

let symbol_collection =
  lazy
    (Feature.require ("help-fns" |> Symbol.intern);
     "help--symbol-completion-table" |> Symbol.intern |> Symbol.function_exn)
;;

let read_function_name =
  (* The code below is based on [describe-function]'s [interactive] spec. *)
  let predicate =
    lazy
      (Defun.lambda
         [%here]
         (Returns Value.Type.bool)
         (let%map_open.Defun () = return ()
          and f = required "f" Symbol.t in
          Symbol.function_is_defined f
          || is_some (Symbol.Property.get Symbol.Property.function_documentation f))
       |> Function.to_value)
  in
  fun ~prompt ~history ->
    let collection = force symbol_collection in
    let predicate = force predicate in
    let%bind name =
      completing_read
        ~prompt
        ~collection
        ~predicate
        ~require_match:True
        ?default:(Point.function_called_at () |> Option.map ~f:Symbol.name)
        ~history
        ()
    in
    if String.is_empty name then raise_s [%message "Did not enter a function name"];
    return name
;;

let read_variable_name =
  let predicate =
    lazy
      (Defun.lambda
         [%here]
         (Returns Value.Type.bool)
         (let%map_open.Defun () = return ()
          and var = required "var" Symbol.t in
          Current_buffer.variable_is_defined var
          || is_some (Symbol.Property.get Symbol.Property.variable_documentation var))
       |> Function.to_value)
  in
  fun ~prompt ~history ->
    let collection = force symbol_collection in
    let predicate = force predicate in
    let%bind name =
      completing_read
        ~prompt
        ~collection
        ~predicate
        ~require_match:True
        ?default:(Point.variable_at () |> Option.map ~f:Symbol.name)
        ~history
        ()
    in
    if String.is_empty name then raise_s [%message "Did not enter a variable name"];
    return name
;;
