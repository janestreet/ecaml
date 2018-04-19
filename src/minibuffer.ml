open! Core_kernel
open! Import

module Q = struct
  include Q
  let minibuffer_exit_hook             = "minibuffer-exit-hook"             |> Symbol.intern
  let minibuffer_history               = "minibuffer-history"               |> Symbol.intern
  let minibuffer_setup_hook            = "minibuffer-setup-hook"            |> Symbol.intern
  let read_from_minibuffer             = "read-from-minibuffer"             |> Symbol.intern
  let y_or_n_p                         = "y-or-n-p"                         |> Symbol.intern
  let y_or_n_p_with_timeout            = "y-or-n-p-with-timeout"            |> Symbol.intern
  let yes_or_no_p                      = "yes-or-no-p"                      |> Symbol.intern
end

let y_or_n ~prompt =
  Symbol.funcall1 Q.y_or_n_p (prompt |> Value.of_utf8_bytes)
  |> Value.to_bool
;;

module Y_or_n_with_timeout = struct
  type 'a t = Y | N | Timeout of 'a [@@deriving sexp_of]
end

let y_or_n_with_timeout ~prompt ~timeout:(span, a) : _ Y_or_n_with_timeout.t =
  let result =
    Symbol.funcall3 Q.y_or_n_p_with_timeout
      (prompt |> Value.of_utf8_bytes)
      (span |> Time_ns.Span.to_sec |> Value.of_float)
      (Q.default_value |> Symbol.to_value) in
  if Value.is_nil result
  then N
  else if Value.equal result Value.t
  then Y
  else Timeout a
;;

let yes_or_no ~prompt =
  Symbol.funcall1 Q.yes_or_no_p (prompt |> Value.of_utf8_bytes)
  |> Value.to_bool
;;

let read_from
      ?default_value
      ?(history_list = Q.minibuffer_history)
      ?history_list_pos
      ?initial_contents
      ()
      ~prompt
  =
  Symbol.funcallN Q.read_from_minibuffer
    [ prompt |> Value.of_utf8_bytes
    ; (match initial_contents with
       | None -> Value.nil
       | Some s -> (s |> Value.of_utf8_bytes))
    ; Value.nil
    ; Value.nil
    ; (match history_list_pos with
       | None -> history_list |> Symbol.to_value
       | Some i -> Value.cons (history_list |> Symbol.to_value) (i |> Value.of_int_exn))
    ; (match default_value with
       | None -> Value.nil
       | Some s -> s |> Value.of_utf8_bytes) ]
  |> Value.to_utf8_bytes_exn
;;

let exit_hook  = Hook.create Normal Q.minibuffer_exit_hook
let setup_hook = Hook.create Normal Q.minibuffer_setup_hook
