open! Core_kernel
open! Import0

module Q = struct
  include Q
  let eval                             = "eval"                             |> Symbol.intern
  let interactive                      = "interactive"                      |> Symbol.intern
  let lambda                           = "lambda"                           |> Symbol.intern
  let let_                             = "let"                              |> Symbol.intern
  let progn                            = "progn"                            |> Symbol.intern
  let read_from_whole_string           = "read-from-whole-string"           |> Symbol.intern
  let thingatpt                        = "thingatpt"                        |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "form"
    let here = [%here]
    let is_in_subtype = fun _ -> true
  end)

let string s = s |> Value.of_utf8_bytes |> of_value_exn

let symbol s = s |> Symbol.to_value |> of_value_exn

let int i = i |> Value.of_int_exn |> of_value_exn

let eval t = Symbol.funcall1 Q.eval (t |> to_value)

let eval_i t = ignore (eval t : Value.t)

let read =
  Feature0.require Q.thingatpt;
  fun string ->
    Symbol.funcall1 Q.read_from_whole_string (string |> Value.of_utf8_bytes)
    |> of_value_exn
;;

let eval_string string = eval (read string)

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
    ; body |> to_value ]
  |> of_value_exn
;;

let lambda =
  let some = Option.some in
  fun ?docstring ?interactive ?optional_args ?rest_arg here ~args ~body ->
    (match docstring with
     | None -> ()
     | Some docstring ->
       if String.mem docstring '\000'
       then (raise_s [%message "docstring contains a NUL byte" (docstring : string)]));
    let args =
      [ args
      ; optional_args |> Option.value_map ~default:[]
                           ~f:(fun optional_args -> Q.A.optional :: optional_args)
      ; rest_arg      |> Option.value_map ~default:[]
                           ~f:(fun rest_arg -> [ Q.A.rest; rest_arg ])]
      |> List.concat
      |> (fun x -> (x : Symbol.t list :> Value.t list))
      |> Value.list
    in
    let here =
      concat [ "Implemented at [";here |> Source_code_position.to_string;"]." ] in
    let docstring =
      match docstring with
      | None -> here
      | Some s -> concat [ s
                         ; if String.is_empty s
                           || String.is_suffix s ~suffix:"\n\n"
                           then ""
                           else if String.is_suffix s ~suffix:"\n"
                           then "\n"
                           else "\n\n"
                         ; here ] in
    [ Q.lambda    |> Symbol.to_value  |> some
    ; args        |> some
    ; docstring   |> Value.of_utf8_bytes |> some
    ; interactive |> Option.map
                       ~f:(fun interactive ->
                         Value.list [ Q.interactive |> Symbol.to_value
                                    ; interactive |> Value.of_utf8_bytes ])
    ; body |> to_value |> some ]
    |> List.filter_opt
    |> Value.list
    |> of_value_exn
;;

let defvar here symbol initial_value ~docstring =
  ignore (
    eval (
      [ Q.defvar      |> Symbol.to_value
      ; symbol        |> Symbol.to_value
      ; initial_value |> q
      ; docstring     |> Value.of_utf8_bytes ]
      |> Value.list
      |> of_value_exn)
    : Value.t);
  Load_history.add_entry here (Var symbol);
;;
