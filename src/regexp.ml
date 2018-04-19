open! Core_kernel
open! Import

module Q = struct
  include Q
  let match_beginning                  = "match-beginning"                  |> Symbol.intern
  let match_data                       = "match-data"                       |> Symbol.intern
  let match_end                        = "match-end"                        |> Symbol.intern
  let match_string                     = "match-string"                     |> Symbol.intern
  let match_string_no_properties       = "match-string-no-properties"       |> Symbol.intern
  let regexp_opt                       = "regexp-opt"                       |> Symbol.intern
  let regexp_quote                     = "regexp-quote"                     |> Symbol.intern
  let set_match_data                   = "set-match-data"                   |> Symbol.intern
  let string_match                     = "string-match"                     |> Symbol.intern
  let string_match_p                   = "string-match-p"                   |> Symbol.intern
end

module Current_buffer = Current_buffer0

include Value.Make_subtype (struct
    let name = "regexp"
    let here = [%here]
    let is_in_subtype = Value.is_string
  end)

let of_pattern string = string |> Value.of_utf8_bytes |> of_value_exn

let to_pattern t = t |> to_value |> Value.to_utf8_bytes_exn

let of_rx rx = of_pattern (Rx.pattern rx)

let match_anything = of_pattern ""
let match_nothing  = of_pattern "z^"

let quote string =
  Symbol.funcall1 Q.regexp_quote (string |> Value.of_utf8_bytes)
  |> of_value_exn
;;

let any_quote strings =
  match strings with
  | [] ->
    (* Elisp's [regexp-opt] returns the wrong regexp for an empty list.  It returns a
       regexp that matches anything, when it should return a regexp that matches
       nothing. *)
    match_nothing
  | _ ->
    Symbol.funcall1 Q.regexp_opt (Value.list (List.map strings ~f:Value.of_utf8_bytes))
    |> of_value_exn
;;

let any_pattern patterns = of_pattern (concat ~sep:{|\||} patterns)

let any ts = any_pattern (List.map ts ~f:to_pattern)

module Last_match = struct
  module Private = struct
    module Location = struct
      type t =
        | Buffer of Buffer.t
        | No_match
        | Text of Text.t
      [@@deriving sexp_of]

      let last = ref No_match
    end
  end

  include Private

  type t =
    { location  : Location.t
    ; positions : Value.t }
  [@@deriving sexp_of]

  let get () =
    match !Location.last with
    | No_match -> None
    | _ as location ->
      Some { location
           ; positions = Symbol.funcall0 Q.match_data }
  ;;

  let get_exn () =
    match get () with
    | Some t -> t
    | None -> raise_s [%message "Prior [Regexp] match did not match"]
  ;;

  let set t =
    Location.last := t.location;
    Symbol.funcall1_i Q.set_match_data t.positions;
  ;;

  let save f =
    let saved = get () in
    protect ~f ~finally:(fun () ->
      match saved with
      | None -> Location.last := No_match
      | Some t -> set t)
  ;;

  let text_exn ?(subexp = 0) ?(text_properties = false) () =
    let result =
      Symbol.funcall2
        (if text_properties
         then Q.match_string
         else Q.match_string_no_properties)
        (subexp |> Value.of_int_exn)
        (match !Location.last with
         | Buffer expected ->
           let current = Current_buffer.get () in
           if not (Buffer.equal current expected)
           then raise_s [%message
                  "[Regexp.Last_match.text_exn] called in wrong buffer"
                    (current : Buffer.t)
                    (expected : Buffer.t)];
           Value.nil
         | No_match -> raise_s [%message "Prior [Regexp] match did not match"]
         | Text text -> text |> Text.to_value) in
    if Value.is_nil result
    then raise_s [%message
           "[Regexp.Last_match.text_exn] got [subexp] that did not match" (subexp : int)];
    result |> Text.of_value_exn
  ;;

  let pos name q =
    fun ?(subexp = 0) () ->
      (match !Location.last with
       | No_match -> raise_s [%message "Prior [Regexp] match did not match"]
       | _ -> ());
      let result = Symbol.funcall1 q (subexp |> Value.of_int_exn) in
      if Value.is_nil result
      then raise_s [%message
             (concat [ "[Regexp.Last_match.";name;"_exn] got [subexp] that did not match" ])
               (subexp : int)];
      result |> Value.to_int_exn
  ;;

  let start_exn = pos "start" Q.match_beginning
  let end_exn   = pos "end"   Q.match_end
end

let match_ ?start ?(update_last_match = false) t text =
  let result =
    Symbol.funcall3
      (if update_last_match
       then Q.string_match
       else Q.string_match_p)
      (t |> to_value)
      (text |> Text.to_value)
      (start |> function
       | None -> Value.nil
       | Some i -> i |> Value.of_int_exn) in
  if Value.is_nil result
  then (
    if update_last_match then Last_match.Location.last := No_match;
    None)
  else (
    if update_last_match then Last_match.Location.last := Text text;
    Some (result |> Value.to_int_exn))
;;

let does_match ?start ?update_last_match t text =
  is_some (match_ t text ?start ?update_last_match)
;;
