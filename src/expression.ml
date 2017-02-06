open! Core
open! Async
open! Import

type t = Value.t [@@deriving sexp_of]

let of_value = Fn.id
let to_value = Fn.id

let string = Value.of_string
let symbol = Symbol.to_value

let eval t = Symbol.funcall Symbol.eval [ t ]

let of_string =
  Symbol.(require ("thingatpt" |> intern));
  let read_from_whole_string = Symbol.intern "read-from-whole-string" in
  fun string ->
    Symbol.funcall read_from_whole_string [ string |> Value.of_string ]
;;

let quote t = Value.list [ symbol Symbol.quote; t ]

let progn ts = Value.list (symbol Symbol.progn :: ts)

let apply t1 t2 = Value.list [ symbol Symbol.apply; t1; t2 ]

let lambda =
  let amper_optional = Symbol.intern "&optional" in
  let amper_rest     = Symbol.intern "&rest"     in
  let some = Option.some in
  fun ?docstring ?interactive ?optional_args ?rest_arg () ~args ~body ->
    (match docstring with
     | None -> ()
     | Some docstring ->
       if String.mem docstring '\000'
       then (raise_s [%message "docstring contains a NUL byte" (docstring : string)]));
    let args =
      [ args
      ; optional_args |> Option.value_map ~default:[]
                           ~f:(fun optional_args -> amper_optional :: optional_args)
      ; rest_arg      |> Option.value_map ~default:[]
                           ~f:(fun rest_arg -> [ amper_rest; rest_arg ])
      ]
      |> List.concat
      |> List.map ~f:symbol
      |> Value.list
    in
    [ Symbol.lambda |> symbol |> some
    ; args          |> some
    ; docstring     |> Option.map ~f:string
    ; interactive   |> Option.map
                         ~f:(fun interactive ->
                           Value.list [ Symbol.interactive |> symbol
                                      ; interactive |> string
                                      ])
    ; body          |> some
    ]
    |> List.filter_opt
    |> Value.list
;;

let combination = Value.list
