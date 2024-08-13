open! Core
open! Import
open Rx_intf

module Q = struct
  include Q

  let alnum = "alnum" |> Symbol.intern
  let alpha = "alpha" |> Symbol.intern
  let any = "any" |> Symbol.intern
  let anything = "anything" |> Symbol.intern
  let digit = "digit" |> Symbol.intern
  let line_end = "line-end" |> Symbol.intern
  let line_start = "line-start" |> Symbol.intern
  let lower = "lower" |> Symbol.intern
  let minimal_match = "minimal-match" |> Symbol.intern
  let maximal_match = "maximal-match" |> Symbol.intern
  let not_ = "not" |> Symbol.intern
  let one_or_more = "one-or-more" |> Symbol.intern
  let or_ = "or" |> Symbol.intern
  let point = "point" |> Symbol.intern
  let seq = "seq" |> Symbol.intern
  let space = "space" |> Symbol.intern
  let submatch = "submatch" |> Symbol.intern
  let submatch_n = "submatch-n" |> Symbol.intern
  let sym_eq = "=" |> Symbol.intern
  let sym_ge = ">=" |> Symbol.intern
  let sym_star_star = "**" |> Symbol.intern
  let upper = "upper" |> Symbol.intern
  let word = "word" |> Symbol.intern
  let xdigit = "xdigit" |> Symbol.intern
  let zero_or_more = "zero-or-more" |> Symbol.intern
  let zero_or_one = "zero-or-one" |> Symbol.intern
end

module Named_char_class = struct
  include Named_char_class

  let to_form t =
    match (t : t) with
    | Alphabetic -> Form.symbol Q.alpha
    | Alphanumeric -> Form.symbol Q.alnum
    | Digit -> Form.symbol Q.digit
    | Hex_digit -> Form.symbol Q.xdigit
    | Lower -> Form.symbol Q.lower
    | Space -> Form.symbol Q.space
    | Upper -> Form.symbol Q.upper
    | Word -> Form.symbol Q.word
  ;;
end

module Char_class = struct
  include Char_class

  let to_form t =
    match (t : t) with
    | Chars_in string -> Form.string string
    | Named named -> Named_char_class.to_form named
    | Range (c1, c2) -> Form.string (sprintf "%c-%c" c1 c2)
  ;;

  let dash_char_set = Char.Set.singleton '-'

  (* This preprocessing prevents us from confusing [a-z] with [-az]. *)
  let to_forms ts =
    let chars, other_ts =
      List.partition_map ts ~f:(function
        | Chars_in s -> First s
        | (Named _ | Range _) as t -> Second t)
    in
    let chars =
      List.fold chars ~init:Char.Set.empty ~f:(fun init s ->
        String.fold s ~init ~f:Set.add)
    in
    let ts =
      let chars_in chars =
        match Set.is_empty chars with
        | true -> []
        | false ->
          let chars = chars |> Set.to_list |> String.of_char_list in
          [ Chars_in chars ]
      in
      List.concat
        [ chars_in (Set.inter chars dash_char_set)
        ; chars_in (Set.diff chars dash_char_set)
        ; other_ts
        ]
    in
    List.map ts ~f:to_form
  ;;
end

module Start_or_end = Start_or_end
include T

let label symbol args = Form.list (Form.symbol symbol :: args)

let rec to_forms ts = List.map ts ~f:to_form

and to_form t =
  match (t : t) with
  | Any_char -> Form.symbol Q.anything
  | Any_in char_classes -> label Q.any (Char_class.to_forms char_classes)
  | Exactly string -> Form.string string
  | Line End -> Form.symbol Q.line_end
  | Line Start -> Form.symbol Q.line_start
  | Minimal_match t -> label Q.minimal_match [ to_form t ]
  | Maximal_match t -> label Q.maximal_match [ to_form t ]
  | None_in char_classes -> label Q.not_ [ to_form (Any_in char_classes) ]
  | One_or_more t -> to_form (Repeat { min = 1; max = None; t })
  | Or ts -> label Q.or_ (to_forms ts)
  | Pattern pattern -> label Q.regexp [ Form.string pattern ]
  | Point -> Form.symbol Q.point
  | Repeat { min; max; t } ->
    (match min, max with
     | 0, None -> label Q.zero_or_more [ to_form t ]
     | 0, Some 1 -> label Q.zero_or_one [ to_form t ]
     | 1, None -> label Q.one_or_more [ to_form t ]
     | n, None -> label Q.sym_ge [ Form.int n; to_form t ]
     | n, Some m ->
       (match Int.( = ) n m with
        | true -> label Q.sym_eq [ Form.int n; to_form t ]
        | false -> label Q.sym_star_star [ Form.int n; Form.int m; to_form t ]))
  | Seq ts -> label Q.seq (to_forms ts)
  | Submatch t -> label Q.submatch [ to_form t ]
  | Submatch_n { index; t } -> label Q.submatch_n [ Form.int index; to_form t ]
  | Zero_or_more t -> to_form (Repeat { min = 0; max = None; t })
  | Zero_or_one t -> to_form (Repeat { min = 0; max = Some 1; t })
;;

let rx_to_string = Funcall.Wrap.("rx-to-string" <: Form.t @-> return string)
let pattern t = rx_to_string (to_form t)
