open! Core
open! Import

module Q = struct
  let end_of_file = "end-of-file" |> Symbol.intern
  let eval = "eval" |> Symbol.intern
  let interactive = "interactive" |> Symbol.intern
  let lambda = "lambda" |> Symbol.intern
  let let_ = "let" |> Symbol.intern
  let progn = "progn" |> Symbol.intern
  let quote = "quote" |> Symbol.intern

  module A = struct
    let optional = "&optional" |> Symbol.intern
    let rest = "&rest" |> Symbol.intern
  end
end

include Value.Make_subtype (struct
    let name = "form"
    let here = [%here]
    let is_in_subtype _ = true
  end)

let string s = s |> Value.of_utf8_bytes |> of_value_exn
let symbol s = s |> Symbol.to_value |> of_value_exn
let int i = i |> Value.of_int_exn |> of_value_exn

let read =
  let read_from_string =
    Funcall.Wrap.(
      "read-from-string" <: string @-> nil_or int @-> return (tuple value int))
  in
  (* This implementation is cribbed from [thingatpt.el]'s [read-from-whole-string].  That
     function was originally exported, but is actually an internal thingatpt function and
     is now obsolete. *)
  fun string ->
    let value, end_of_first_read = read_from_string string None in
    match read_from_string string (Some end_of_first_read) with
    | exception Value.For_testing.Elisp_signal { symbol; data = _ }
      when Value.eq symbol (Symbol.to_value Q.end_of_file) ->
      (* Unfortunately, we can't distinguish between these two reasons for the second read
         signaling end-of-error:

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

module Blocking = struct
  let eval t = Symbol.funcall1 Q.eval (t |> to_value)
  let eval_i t = ignore (eval t : Value.t)
  let eval_string string = eval (read string)
end

let eval t = Value.Private.run_outside_async [%here] (fun () -> Blocking.eval t)
let eval_i t = Value.Private.run_outside_async [%here] (fun () -> Blocking.eval_i t)

let eval_string t =
  Value.Private.run_outside_async [%here] (fun () -> Blocking.eval_string t)
;;

let list ts = Value.list (ts : t list :> Value.t list) |> of_value_exn
let nil = list []
let q value = Value.list [ Symbol.to_value Q.quote; value ]
let quote value = q value |> of_value_exn
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

let lambda =
  let some = Option.some in
  fun ?docstring ?interactive ?optional_args ?rest_arg here ~args ~body ->
    (match docstring with
     | None -> ()
     | Some docstring ->
       if String.mem docstring '\000'
       then raise_s [%message "docstring contains a NUL byte" (docstring : string)]);
    let args =
      [ args
      ; (match optional_args with
         | None | Some [] -> []
         | Some optional_args -> Q.A.optional :: optional_args)
      ; rest_arg
        |> Option.value_map ~default:[] ~f:(fun rest_arg -> [ Q.A.rest; rest_arg ])
      ]
      |> List.concat
      |> (fun x -> (x : Symbol.t list :> Value.t list))
      |> Value.list
    in
    let here =
      concat [ "Implemented at ["; here |> Source_code_position.to_string; "]." ]
    in
    let docstring =
      match docstring with
      | None -> here
      | Some s ->
        let s = String.strip s in
        concat [ (if String.is_empty s then "" else concat [ s; "\n\n" ]); here ]
    in
    [ Q.lambda |> Symbol.to_value |> some
    ; args |> some
    ; docstring |> Value.of_utf8_bytes |> some
    ; interactive
      |> Option.map ~f:(fun interactive ->
        Value.list [ Q.interactive |> Symbol.to_value; interactive ])
    ; body |> to_value |> some
    ]
    |> List.filter_opt
    |> Value.list
    |> of_value_exn
;;
