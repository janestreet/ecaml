open! Core_kernel
open! Import

module Q = struct
  include Q

  let minibuffer_exit_hook = "minibuffer-exit-hook" |> Symbol.intern
  and minibuffer_history = "minibuffer-history" |> Symbol.intern
  and minibuffer_setup_hook = "minibuffer-setup-hook" |> Symbol.intern
  and read_from_minibuffer = "read-from-minibuffer" |> Symbol.intern
  and y_or_n_p = "y-or-n-p" |> Symbol.intern
  and y_or_n_p_with_timeout = "y-or-n-p-with-timeout" |> Symbol.intern
  and yes_or_no_p = "yes-or-no-p" |> Symbol.intern
end

module Y_or_n_with_timeout = struct
  type 'a t =
    | Y
    | N
    | Timeout of 'a
  [@@deriving sexp_of]
end

module Blocking = struct
  let y_or_n ~prompt =
    Symbol.funcall1 Q.y_or_n_p (prompt |> Value.of_utf8_bytes) |> Value.to_bool
  ;;

  let y_or_n_with_timeout ~prompt ~timeout:(span, a) : _ Y_or_n_with_timeout.t =
    let result =
      Symbol.funcall3
        Q.y_or_n_p_with_timeout
        (prompt |> Value.of_utf8_bytes)
        (span |> Time_ns.Span.to_sec |> Value.of_float)
        (Q.default_value |> Symbol.to_value)
    in
    if Value.is_nil result
    then N
    else if Value.equal result Value.t
    then Y
    else Timeout a
  ;;

  let yes_or_no ~prompt =
    Symbol.funcall1 Q.yes_or_no_p (prompt |> Value.of_utf8_bytes) |> Value.to_bool
  ;;

  let minibuffer_history = Var.create Q.minibuffer_history Value.Type.(list string)

  let read_from
        ?default_value
        ?(history = minibuffer_history)
        ?history_pos
        ?initial_contents
        ()
        ~prompt
    =
    let history = history.symbol |> Symbol.to_value in
    Symbol.funcallN
      Q.read_from_minibuffer
      [ prompt |> Value.of_utf8_bytes
      ; (match initial_contents with
         | None -> Value.nil
         | Some s -> s |> Value.of_utf8_bytes)
      ; Value.nil
      ; Value.nil
      ; (match history_pos with
         | None -> history
         | Some i -> Value.cons history (i |> Value.of_int_exn))
      ; (match default_value with
         | None -> Value.nil
         | Some s -> s |> Value.of_utf8_bytes)
      ]
    |> Value.to_utf8_bytes_exn
  ;;
end

let y_or_n ~prompt =
  Async_ecaml.Private.run_outside_async (fun () -> Blocking.y_or_n ~prompt)
;;

let y_or_n_with_timeout ~prompt ~timeout =
  Async_ecaml.Private.run_outside_async (fun () ->
    Blocking.y_or_n_with_timeout ~prompt ~timeout)
;;

let yes_or_no ~prompt =
  Async_ecaml.Private.run_outside_async (fun () -> Blocking.yes_or_no ~prompt)
;;

let read_from ?default_value ?history ?history_pos ?initial_contents () ~prompt =
  Async_ecaml.Private.run_outside_async (fun () ->
    Blocking.read_from
      ?default_value
      ?history
      ?history_pos
      ?initial_contents
      ()
      ~prompt)
;;

let exit_hook = Hook.create Q.minibuffer_exit_hook ~hook_type:Normal
let setup_hook = Hook.create Q.minibuffer_setup_hook ~hook_type:Normal
