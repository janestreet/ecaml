open! Core
open! Async_kernel
open! Import

module Q = struct
  include Q

  let default_value = "default-value" |> Symbol.intern
end

module Y_or_n_with_timeout = struct
  type 'a t =
    | Y
    | N
    | Timeout of 'a
  [@@deriving sexp_of]
end

module History = struct
  type t = T of string list Var.t [@@deriving sexp_of]

  let symbol (T t) = Var.symbol t

  let create symbol ?(docstring = "A minibuffer history list.") here =
    T
      (Defvar.defvar
         symbol
         here
         ~docstring
         ~type_:Value.Type.(list string)
         ~initial_value:[]
         ~include_in_all_defvar_symbols:false
         ())
  ;;

  let all_by_symbol_name = Hashtbl.create (module String)

  let find_or_create symbol ?docstring here =
    Hashtbl.find_or_add all_by_symbol_name (Symbol.name symbol) ~default:(fun () ->
      create symbol ?docstring here)
  ;;
end

let history : History.t = T Var.Wrap.("minibuffer-history" <: list string)

module History_length = struct
  type t =
    | Truncate_after of int
    | No_truncation
  [@@deriving sexp_of]

  let of_value_exn value =
    if Value.is_integer value
    then Truncate_after (Value.to_int_exn value)
    else if Value.eq Value.t value
    then No_truncation
    else
      raise_s [%sexp "Could not translate value to History_length.t", (value : Value.t)]
  ;;

  let to_value = function
    | Truncate_after i -> Value.of_int_exn i
    | No_truncation -> Value.t
  ;;

  let t = Value.Type.create [%sexp "history-length"] [%sexp_of: t] of_value_exn to_value
end

let history_length = Customization.Wrap.("history-length" <: History_length.t)

let y_or_n =
  let y_or_n_p = Funcall.Wrap.("y-or-n-p" <: string @-> return bool) in
  fun ~prompt -> Async_ecaml.Private.run_outside_async [%here] (fun () -> y_or_n_p prompt)
;;

include struct
  open struct
    let y_or_n_p_with_timeout =
      Funcall.Wrap.(
        "y-or-n-p-with-timeout" <: string @-> float @-> Symbol.t @-> return value)
    ;;
  end

  let y_or_n_with_timeout ~prompt ~timeout:(span, a) =
    Async_ecaml.Private.run_outside_async [%here] (fun () ->
      let result =
        y_or_n_p_with_timeout prompt (span |> Time_ns.Span.to_sec) Q.default_value
      in
      if Value.is_nil result
      then Y_or_n_with_timeout.N
      else if Value.equal result Value.t
      then Y
      else Timeout a)
  ;;
end

let yes_or_no =
  let yes_or_no_p = Funcall.Wrap.("yes-or-no-p" <: string @-> return bool) in
  fun ~prompt ->
    Async_ecaml.Private.run_outside_async [%here] (fun () -> yes_or_no_p prompt)
;;

let read_from =
  let read_from_minibuffer =
    Funcall.Wrap.(
      "read-from-minibuffer"
      <: string
         @-> nil_or string
         @-> nil_or Keymap.t
         @-> bool
         @-> value
         @-> nil_or string
         @-> return string)
  in
  fun ~prompt ?initial_contents ?default_value ~history ?history_pos () ->
    Async_ecaml.Private.run_outside_async [%here] (fun () ->
      let history = History.symbol history |> Symbol.to_value in
      read_from_minibuffer
        prompt
        initial_contents
        None
        false
        (match history_pos with
         | None -> history
         | Some i -> Value.cons history (i |> Value.of_int_exn))
        default_value)
;;

let read_file_name =
  let read_file_name_from_minbuffer =
    Funcall.Wrap.(
      "read-file-name"
      <: string
         @-> nil_or string
         @-> nil_or string
         @-> nil_or string
         @-> nil_or string
         @-> nil_or Function.t
         @-> return string)
  in
  fun ~prompt ?directory ?default_filename ?mustmatch ?initial ?predicate () ->
    Async_ecaml.Private.run_outside_async [%here] (fun () ->
      read_file_name_from_minbuffer
        prompt
        directory
        default_filename
        mustmatch
        initial
        predicate)
;;

let exit_hook = Hook.Wrap.("minibuffer-exit-hook" <: Normal_hook)
let setup_hook = Hook.Wrap.("minibuffer-setup-hook" <: Normal_hook)

let active_window =
  Funcall.Wrap.("active-minibuffer-window" <: nullary @-> return (nil_or Window.t))
;;

let prompt = Funcall.Wrap.("minibuffer-prompt" <: nullary @-> return (nil_or string))

let exit =
  let exit_minibuffer = Funcall.Wrap.("exit-minibuffer" <: nullary @-> return nil) in
  fun () ->
    exit_minibuffer ();
    assert false
;;

let depth = Funcall.Wrap.("minibuffer-depth" <: nullary @-> return int)
let contents = Funcall.Wrap.("minibuffer-contents" <: nullary @-> return string)
