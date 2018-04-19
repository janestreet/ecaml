open! Core_kernel
open! Import

module Q = struct
  include Q
  let copy_syntax_table                = "copy-syntax-table"                |> Symbol.intern
  let make_syntax_table                = "make-syntax-table"                |> Symbol.intern
  let modify_syntax_entry              = "modify-syntax-entry"              |> Symbol.intern
  let standard_syntax_table            = "standard-syntax-table"            |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "syntax-table"
    let here = [%here]
    let is_in_subtype = Value.is_syntax_table
  end)

let equal = eq

let standard = Symbol.funcall0 Q.standard_syntax_table |> of_value_exn

let create ?parent () =
  Symbol.funcall1 Q.make_syntax_table
    (match parent with
     | None -> Value.nil
     | Some t -> (t |> to_value))
  |> of_value_exn
;;

let copy t = Symbol.funcall1 Q.copy_syntax_table (t |> to_value) |> of_value_exn

module Class = struct
  module T = struct
    type t =
      | Char_quote
      | Close_paren
      | Comment_end
      | Comment_start
      | Escape
      | Expression_prefix
      | Generic_comment_delimiter
      | Generic_string_delimiter
      | Inherit_standard
      | Open_paren
      | Paired
      | Punctuation
      | String_quote
      | Symbol_constitutent
      | Whitespace
      | Word_constituent
    [@@deriving compare, enumerate, hash, sexp_of]
  end

  include T
  include Hashable.Make_plain (T)

  let equal = [%compare.equal: t]

  let to_string t = [%sexp (t : t)] |> Sexp.to_string

  let to_char = function
    | Char_quote                -> '/'
    | Close_paren               -> ')'
    | Comment_end               -> '>'
    | Comment_start             -> '<'
    | Escape                    -> '\\'
    | Expression_prefix         -> '\''
    | Generic_comment_delimiter -> '!'
    | Generic_string_delimiter  -> '|'
    | Inherit_standard          -> '@'
    | Open_paren                -> '('
    | Paired                    -> '$'
    | Punctuation               -> '.'
    | String_quote              -> '"'
    | Symbol_constitutent       -> '_'
    | Whitespace                -> ' '
    | Word_constituent          -> 'w'
  ;;

  let to_char_code t = t |> to_char |> Char_code.of_char_exn

  let t_by_char_code =
    lazy (
      let index t = t |> to_char_code |> Char_code.to_int in
      let max_index = List.fold all ~init:0 ~f:(fun ac t -> Int.max ac (index t)) in
      let t_by_char_code = Option_array.create ~len:(max_index + 1) in
      List.iter all ~f:(fun t -> Option_array.set_some t_by_char_code (index t) t);
      t_by_char_code)
  ;;

  let of_char_code_exn char_code =
    match Option_array.get (force t_by_char_code) (char_code |> Char_code.to_int) with
    | Some t -> t
    | None ->
      raise_s [%message
        "[Syntax_table.Class.of_char_code_exn] got unknown char code"
          (char_code : Char_code.t)]
  ;;
end

module Flag = struct
  type t =
    | Alternative_comment
    | Commend_end_first_char
    | Comment_end_second_char
    | Comment_start_first_char
    | Comment_start_second_char
    | Nested
    | Prefix_char
  [@@deriving enumerate, sexp_of]

  let to_char = function
    | Alternative_comment       -> 'b'
    | Commend_end_first_char    -> '3'
    | Comment_end_second_char   -> '4'
    | Comment_start_first_char  -> '1'
    | Comment_start_second_char -> '2'
    | Nested                    -> 'n'
    | Prefix_char               -> 'p'
  ;;
end

module Descriptor = struct
  type t = Class.t * Flag.t list
  [@@deriving sexp_of]

  let to_value (class_, flags) =
    let s = Bytes.create (1 + List.length flags) in
    Bytes.set s 0 (class_ |> Class.to_char);
    List.iteri flags ~f:(fun i flag -> Bytes.set s (i + 1) (flag |> Flag.to_char));
    s |> Bytes.to_string |> Value.of_utf8_bytes
  ;;
end

let set t char_code class_ flags =
  Symbol.funcall3_i Q.modify_syntax_entry
    (char_code |> Char_code.to_value)
    ((class_, flags) |> Descriptor.to_value)
    (t |> to_value);
;;

let set_char t char class_ flags = set t (char |> Char_code.of_char_exn) class_ flags
