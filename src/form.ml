open! Core_kernel
open! Import

type t = Value.t [@@deriving sexp_of]

let of_value = Fn.id
let to_value = Fn.id

let string = Value.of_utf8_bytes

let symbol = Symbol.to_value

let eval t = Symbol.funcall1 Q.eval t

let read =
  Feature.require Q.thingatpt;
  fun string ->
    Symbol.funcall1 Q.read_from_whole_string (string |> Value.of_utf8_bytes)
;;

let quote t = Value.list [ symbol Q.quote; t ]

let progn ts = Value.list (symbol Q.progn :: ts)

let apply t1 t2 = Value.list [ symbol Q.apply; t1; t2 ]

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
      |> List.map ~f:symbol
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
    [ Q.lambda    |> symbol  |> some
    ; args        |> some
    ; docstring   |> string |> some
    ; interactive |> Option.map
                       ~f:(fun interactive ->
                         Value.list [ Q.interactive |> symbol
                                    ; interactive |> string ])
    ; body                |> some ]
    |> List.filter_opt
    |> Value.list
;;

let list = Value.list
