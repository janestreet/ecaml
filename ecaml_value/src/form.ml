open! Core
open! Import

module Q = struct
  let end_of_file = "end-of-file" |> Value.intern
  let eval = "eval" |> Symbol.intern
  let let_ = "let" |> Symbol.intern
  let progn = "progn" |> Symbol.intern
  let quote = "quote" |> Symbol.intern
  let read_from_string = "read-from-string" |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "form"
    let here = [%here]
    let is_in_subtype _ = true
  end)

let string s = s |> Value.of_utf8_bytes |> of_value_exn
let symbol s = s |> Symbol.to_value |> of_value_exn
let int i = i |> Value.of_int_exn |> of_value_exn

let read_from_string ?start string =
  (* Disable profiling since serializing the string can be quite expensive. *)
  Symbol.funcall2
    Q.read_from_string
    string
    (Option.value_map start ~f:Value.of_int_exn ~default:Value.nil)
    ~should_profile:false
  |> Value.Type.(of_value_exn (tuple value int))
;;

let read
  (* This implementation is cribbed from [thingatpt.el]'s [read-from-whole-string]. That
     function was originally exported, but is actually an internal thingatpt function and
     is now obsolete. *)
    string
  =
  let string_value = Value.of_utf8_bytes string in
  let value, end_of_first_read = read_from_string string_value in
  match read_from_string string_value ~start:end_of_first_read with
  | exception exn when Value.Expert.has_error_condition exn Q.end_of_file ->
    (* Unfortunately, we can't distinguish between these two reasons for the second read
       signaling end-of-file:

       1. There's nothing after the first form.
       2. There's an unbalanced opening delimiter after the first form.

       E.g., [(read-from-string "")] and [(read-from-string "(")] do not produce
       distinguishable results. *)
    of_value_exn value
  | exception exn ->
    raise_s
      [%message
        "Raised while scanning for trailing data"
          (string : string)
          (end_of_first_read : int)
          (exn : exn)]
  | _ ->
    raise_s
      [%message "Trailing data in string" (string : string) (end_of_first_read : int)]
;;

let read_buffer buffer =
  let value, _ = read_from_string (Value.of_buffer_contents buffer) in
  of_value_exn value
;;

module Blocking = struct
  let eval t = Symbol.funcall1 Q.eval (t |> to_value)
  let eval_i t = ignore (eval t : Value.t)
  let eval_string string = eval (read string)
end

let eval t = Value.Private.run_outside_async (fun () -> Blocking.eval t)
let eval_i t = Value.Private.run_outside_async (fun () -> Blocking.eval_i t)
let eval_string t = Value.Private.run_outside_async (fun () -> Blocking.eval_string t)
let list ts = Value.list (ts : t list :> Value.t list) |> of_value_exn
let nil = list []
let bool b = if b then of_value_exn Value.t else nil
let q value = Value.list [ Symbol.to_value Q.quote; value ]
let quote value = q value |> of_value_exn
let quoted_symbol sym = quote (Symbol.to_value sym)
let progn ts = list (symbol Q.progn :: ts)

let let_ bindings body =
  Value.list
    [ Q.let_ |> Symbol.to_value
    ; bindings
      |> List.map ~f:(fun (v, e) -> Value.list [ v |> Symbol.to_value; e |> to_value ])
      |> Value.list
    ; body |> to_value
    ]
  |> of_value_exn
;;

let apply fn args = list (symbol fn :: args)
