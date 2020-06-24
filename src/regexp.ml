open! Core_kernel
open! Import
module Current_buffer = Current_buffer0

module Strict = struct
  include Value.Make_subtype (struct
      let name = "regexp"
      let here = [%here]
      let is_in_subtype = Value.is_string
    end)

  let of_pattern string = string |> Value.of_utf8_bytes |> of_value_exn
end

type t = Strict.t Lazy.t [@@deriving sexp_of]

let of_value_exn value = value |> Strict.of_value_exn |> Lazy.from_val
let to_value t = t |> force |> Strict.to_value
let type_ = Value.Type.create [%message "Regexp.t"] [%sexp_of: t] of_value_exn to_value
let t = type_
let of_pattern string = string |> Strict.of_pattern |> Lazy.from_val
let to_pattern t = t |> to_value |> Value.to_utf8_bytes_exn
let of_rx rx = lazy (Strict.of_pattern (Rx.pattern rx))
let match_anything = of_pattern ""
let match_nothing = of_pattern "z^"

let quote =
  let quote = Funcall.Wrap.("regexp-quote" <: string @-> return Strict.t) in
  fun string -> lazy (quote string)
;;

let any_quote =
  let regexp_opt = Funcall.Wrap.("regexp-opt" <: list string @-> return Strict.t) in
  fun strings ->
    match strings with
    | [] ->
      (* Elisp's [regexp-opt] returns the wrong regexp for an empty list.  It returns a
         regexp that matches anything, when it should return a regexp that matches
         nothing. *)
      match_nothing
    | _ -> lazy (regexp_opt strings)
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
    { location : Location.t
    ; positions : Value.t
    }
  [@@deriving sexp_of]

  let match_data = Funcall.Wrap.("match-data" <: nullary @-> return value)

  let get () =
    match !Location.last with
    | No_match -> None
    | _ as location -> Some { location; positions = match_data () }
  ;;

  let get_exn () =
    match get () with
    | Some t -> t
    | None -> raise_s [%message "Prior [Regexp] match did not match"]
  ;;

  let set_match_data = Funcall.Wrap.("set-match-data" <: value @-> return nil)

  let set t =
    Location.last := t.location;
    set_match_data t.positions
  ;;

  let save f =
    let saved = get () in
    protect ~f ~finally:(fun () ->
      match saved with
      | None -> Location.last := No_match
      | Some t -> set t)
  ;;

  let match_string =
    Funcall.Wrap.("match-string" <: int @-> nil_or Text.t @-> return (nil_or Text.t))
  ;;

  let match_string_no_properties =
    Funcall.Wrap.(
      "match-string-no-properties" <: int @-> nil_or Text.t @-> return (nil_or Text.t))
  ;;

  let text_exn ?(subexp = 0) ?(text_properties = false) () =
    let result =
      (if text_properties then match_string else match_string_no_properties)
        subexp
        (match !Location.last with
         | Buffer expected ->
           let current = Current_buffer.get () in
           if not (Buffer.equal current expected)
           then
             raise_s
               [%message
                 "[Regexp.Last_match.text_exn] called in wrong buffer"
                   (current : Buffer.t)
                   (expected : Buffer.t)];
           None
         | No_match -> raise_s [%message "Prior [Regexp] match did not match"]
         | Text text -> Some text)
    in
    match result with
    | Some x -> x
    | None ->
      raise_s
        [%message
          "[Regexp.Last_match.text_exn] got [subexp] that did not match" (subexp : int)]
  ;;

  let pos name f ?(subexp = 0) () =
    (match !Location.last with
     | No_match -> raise_s [%message "Prior [Regexp] match did not match"]
     | _ -> ());
    match f subexp with
    | Some i -> i
    | None ->
      raise_s
        [%message
          (concat [ "[Regexp.Last_match."; name; "_exn] got [subexp] that did not match" ])
            (subexp : int)]
  ;;

  let start_exn =
    pos "start" Funcall.Wrap.("match-beginning" <: int @-> return (nil_or int))
  ;;

  let end_exn = pos "end" Funcall.Wrap.("match-end" <: int @-> return (nil_or int))

  let replace_match =
    Funcall.Wrap.(
      "replace-match"
      <: string @-> bool @-> bool @-> nil_or string @-> nil_or int @-> return nil)
  ;;

  let replace str = replace_match str false false None None
end

let string_match =
  Funcall.Wrap.("string-match" <: t @-> Text.t @-> nil_or int @-> return (nil_or int))
;;

let string_match_p =
  Funcall.Wrap.("string-match-p" <: t @-> Text.t @-> nil_or int @-> return (nil_or int))
;;

let match_ ?start ?(update_last_match = false) t text =
  let result =
    (if update_last_match then string_match else string_match_p) t text start
  in
  if update_last_match
  then Last_match.Location.last := if is_none result then No_match else Text text;
  result
;;

let does_match ?start ?update_last_match t text =
  is_some (match_ t text ?start ?update_last_match)
;;

let save_match_data = Save_wrappers.save_match_data

let extract ?start ?subexp t text =
  save_match_data Sync (fun () ->
    match does_match ?start ~update_last_match:true t text with
    | false -> None
    | true ->
      let text = Last_match.text_exn ?subexp ~text_properties:false () in
      Some (text |> Text.to_utf8_bytes))
;;

let extract_string ?start ?subexp t s = extract ?start ?subexp t (Text.of_utf8_bytes s)

let replace_regexp_in_string =
  Funcall.Wrap.("replace-regexp-in-string" <: t @-> Text.t @-> Text.t @-> return Text.t)
;;

let replace t ~with_ ~in_ = replace_regexp_in_string t with_ in_

let replace_string t ~with_ ~in_ =
  replace_regexp_in_string t (with_ |> Text.of_utf8_bytes) (in_ |> Text.of_utf8_bytes)
  |> Text.to_utf8_bytes
;;
