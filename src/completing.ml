open! Core
open! Async_kernel
open! Import
include (val Symbol_prefix.extend symbol_prefix "completing")

module Q = struct
  include Q

  let confirm = "confirm" |> Symbol.intern
  let confirm_after_completion = "confirm-after-completion" |> Symbol.intern
end

module Initial_input = struct
  include Minibuffer.Initial_input

  let t = completing_t
end

module Require_match = struct
  type t =
    | Confirm
    | Confirm_after_completion
    | False
    | Require_match_or_null__confirm_if_ret_completes
    | Require_match_or_complete_or_null

  let type_ =
    Value.Type.map
      Symbol.t
      ~name:[%sexp "completing", "require-match"]
      ~of_:(fun symbol ->
        List.Assoc.find
          [ Q.confirm, Confirm
          ; Q.confirm_after_completion, Confirm_after_completion
          ; Q.nil, False
          ; Q.t, Require_match_or_complete_or_null
          ]
          symbol
          ~equal:Symbol.equal
        |> Option.value ~default:Require_match_or_null__confirm_if_ret_completes)
      ~to_:(function
        | Confirm -> Q.confirm
        | Confirm_after_completion -> Q.confirm_after_completion
        | False -> Q.nil
        | Require_match_or_null__confirm_if_ret_completes -> Symbol.intern "other"
        | Require_match_or_complete_or_null -> Q.t)
  ;;

  let t = type_
  let of_value_exn = Value.Type.of_value_exn type_
  let to_value = Value.Type.to_value type_
  let default = False
end

module Programmed_completion = struct
  include (val Symbol_prefix.extend symbol_prefix "programmed-completion")

  module Operation = struct
    include (val Symbol_prefix.extend symbol_prefix "operation")

    type t =
      | Metadata
      | Other of Value.t
    [@@deriving sexp_of]

    include Valueable.Make (struct
        type nonrec t = t

        let type_ =
          let metadata_value = Symbol.intern "metadata" |> Symbol.to_value in
          Value.Type.create
            [%message elisp_name]
            [%sexp_of: t]
            (fun value ->
              match Value.equal value metadata_value with
              | true -> Metadata
              | false -> Other value)
            (function
              | Metadata -> metadata_value
              | Other value -> value)
        ;;
      end)
  end

  module Metadata = struct
    type t =
      { annotation_function : (string -> string) option
      ; display_sort_function : (string list -> string list) option
      }
    [@@deriving fields ~iterators:create ~direct_iterators:to_list]

    let create = Fields.create

    let to_value =
      let metadatum_type = Value.Type.(tuple Symbol.t Function.t) in
      let f ~arg ~returns field _t f =
        let%map.Option f in
        let name = Enum.Single.to_string_hum (module String) (Field.name field) in
        let function_ =
          Defun.lambda
            [%here]
            (Returns returns)
            (let%map_open.Defun arg in
             f arg)
        in
        Value.Type.to_value metadatum_type (Symbol.intern name, function_)
      in
      Fields.Direct.to_list
        ~annotation_function:
          (f ~arg:Defun.(required "CANDIDATE" string) ~returns:Value.Type.string)
        ~display_sort_function:
          (f
             ~arg:Defun.(required "CANDIDATES" (list string))
             ~returns:Value.Type.(list string))
      >> List.filter_opt
      >> List.cons (Operation.to_value Metadata)
      >> Value.list
    ;;
  end

  type t =
    | Symbol of Symbol.t
    | T of
        { collection : string list
        ; metadata : Metadata.t
        }

  let completion_table_dynamic =
    Funcall.Wrap.("completion-table-dynamic" <: Function.t @-> return Function.t)
  ;;

  let to_value = function
    | Symbol symbol -> Symbol.function_exn symbol
    | T { collection; metadata } ->
      (* Using the full "Programmed Completion" interface is necessary to provide
         annotation/sorting functions, but Emacs provides [completion_table_dynamic] to
         create a basic implementation of the interface, which we can delegate to for
         everything except adding our metadata. *)
      let dynamic =
        let value = Value.Type.(to_value (list string)) collection in
        completion_table_dynamic
          (Defun.lambda
             [%here]
             (Returns Value.Type.value)
             (let%map_open.Defun () = required "PREFIX" ignored in
              value))
      in
      Defun.lambda
        [%here]
        (Returns Value.Type.value)
        (let%map_open.Defun () = return ()
         and prefix = required "PREFIX" value
         and predicate = required "PREDICATE" value
         and operation = required "OPERATION" Operation.t in
         match operation with
         | Metadata -> Metadata.to_value metadata
         | Other operation ->
           Value.funcall3 (Function.to_value dynamic) prefix predicate operation)
      |> Function.to_value
  ;;

  let create (collection : string list) ~annotation_function ~display_sort_function =
    let metadata = Metadata.create ~annotation_function ~display_sort_function in
    T { collection; metadata }
  ;;
end

let read_raw =
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
  fun ~prompt_no_colon
    ~collection
    ?(predicate = Value.nil)
    ?(require_match = Require_match.default)
    ?(initial_input = Initial_input.Empty)
    ?default
    ~history
    () ->
    Async_ecaml.Private.run_outside_async (fun () ->
      let formatted_prompt = Minibuffer.format_prompt ~prompt_no_colon ~default in
      completing_read
        formatted_prompt
        collection
        predicate
        require_match
        initial_input
        history
        default)
;;

let completing_read
  ~prompt_no_colon
  ~programmed_completion
  ?predicate
  ?require_match
  ?initial_input
  ?default
  ~history
  ()
  =
  read_raw
    ~prompt_no_colon
    ~collection:(Programmed_completion.to_value programmed_completion)
    ?predicate
    ?require_match
    ?initial_input
    ~history:(Minibuffer.History.symbol history)
    ?default
    ()
;;

let read
  ~prompt_no_colon
  ~collection
  ?annotation_function
  ?display_sort_function
  ?require_match
  ?initial_input
  ?default
  ~history
  ()
  =
  completing_read
    ~prompt_no_colon
    ~programmed_completion:
      (Programmed_completion.create
         collection
         ~annotation_function
         ~display_sort_function)
    ?require_match
    ?initial_input
    ?default
    ~history
    ()
;;

let read_map_key
  ~prompt_no_colon
  ~collection
  ?annotation_function
  ?display_sort_function
  ?initial_input
  ?default
  ?(confirm_ret_completion = false)
  ~history
  ()
  =
  let%bind choice =
    read
      ~prompt_no_colon
      ~collection:(Map.keys collection)
      ~require_match:
        (if confirm_ret_completion
         then Require_match_or_null__confirm_if_ret_completes
         else Require_match_or_complete_or_null)
      ?annotation_function
      ?display_sort_function
      ?initial_input
      ?default
      ~history
      ()
  in
  match Map.find collection choice with
  | Some data -> return data
  | None ->
    (match choice with
     | "" ->
       (* [read] can return an empty string even if [~require_match:True] is passed and
          the empty string is not in the collection. This happens when [default] is not
          supplied; otherwise, pressing RET at an empty minibuffer would return the
          default instead. *)
       raise_s [%message "Did not enter a valid choice"]
     | choice ->
       raise_s
         [%message
           "Ecaml bug: [Completion.read_map_key] could not find key in map"
             (choice : string)
             ~keys:(Map.keys collection : string list)])
;;

let crm_separator = Var.Wrap.("crm-separator" <: string)

let read_multiple =
  let completing_read_multiple =
    Funcall.Wrap.(
      "completing-read-multiple"
      <: string
         @-> list string
         @-> value
         @-> Require_match.t
         @-> Initial_input.t
         @-> Symbol.t
         @-> nil_or string
         @-> return (list string))
  in
  fun ~prompt_no_colon
    ~collection
    ?(require_match = Require_match.False)
    ?(separator_regexp = "[ \t]*,[ \t]*")
    ?(initial_input = Initial_input.Empty)
    ?default
    ~history
    () ->
    Async_ecaml.Private.run_outside_async (fun () ->
      let predicate = Value.nil in
      let formatted_prompt = Minibuffer.format_prompt ~prompt_no_colon ~default in
      Current_buffer.set_value_temporarily
        Sync
        crm_separator
        separator_regexp
        ~f:(fun () ->
          completing_read_multiple
            formatted_prompt
            collection
            predicate
            require_match
            initial_input
            (Minibuffer.History.symbol history)
            default))
;;

let read_multiple_map_keys
  ~prompt_no_colon
  ~collection
  ?separator_regexp
  ?initial_input
  ?default
  ~history
  ()
  =
  let%bind choices =
    read_multiple
      ~prompt_no_colon
      ~collection:(Map.keys collection)
      ~require_match:Require_match_or_complete_or_null
      ?separator_regexp
      ?initial_input
      ?default
      ~history
      ()
  in
  return (List.map choices ~f:(Map.find_exn collection))
;;

let symbol_collection =
  lazy
    (Feature.require ("help-fns" |> Symbol.intern);
     "help--symbol-completion-table" |> Symbol.intern)
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
  fun ~prompt_no_colon ~history ->
    let collection = force symbol_collection in
    let predicate = force predicate in
    let%bind name =
      completing_read
        ~prompt_no_colon
        ~programmed_completion:(Symbol collection)
        ~predicate
        ~require_match:Require_match_or_complete_or_null
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
  fun ~prompt_no_colon ~history ->
    let collection = force symbol_collection in
    let predicate = force predicate in
    let%bind name =
      completing_read
        ~prompt_no_colon
        ~programmed_completion:(Symbol collection)
        ~predicate
        ~require_match:Require_match_or_complete_or_null
        ?default:(Point.variable_at () |> Option.map ~f:Symbol.name)
        ~history
        ()
    in
    if String.is_empty name then raise_s [%message "Did not enter a variable name"];
    return name
;;
