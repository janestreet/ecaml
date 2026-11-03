open! Core
open! Async_kernel
open! Import

(* Calling Elisp's [identity] with this wrapped type round-trips the sexp through Elisp:
   OCaml to Elisp via [to_form_elisp], and back via [prin1] + [of_prin1_output]. *)
let round_trip_through_elisp =
  Funcall.Wrap.("identity" <: Ecaml_sexp.t @-> return Ecaml_sexp.t)
;;

let ecaml_sexp_pp_to_string =
  Funcall.Wrap.("ecaml-sexp-pp-to-string" <: Ecaml_sexp.t @-> return string)
;;

let ecaml_sexp_pp_value_to_string =
  Funcall.Wrap.("ecaml-sexp-pp-to-string" <: value @-> return string)
;;

let%expect_test "atoms become multibyte text in Elisp" =
  let car = Funcall.Wrap.("car" <: value @-> return value) in
  let multibyte_string_p = Funcall.Wrap.("multibyte-string-p" <: value @-> return bool) in
  let atom_in_elisp string =
    Value.Type.to_value Ecaml_sexp.t [%sexp [ (string : string) ]] |> car
  in
  (* A valid-UTF-8 atom converts to a multibyte Elisp string that is [equal] to the
     ordinary Elisp string containing the same text. *)
  let sparkles = atom_in_elisp "✨" in
  require (multibyte_string_p sparkles);
  require (Value.equal sparkles (Value.of_utf8_bytes "✨"));
  (* An atom holding non-UTF-8 bytes converts to raw bytes, and still round-trips. *)
  let invalid_utf8 = "\xff\xfe" in
  require (not (multibyte_string_p (atom_in_elisp invalid_utf8)));
  let input = [%sexp [ (invalid_utf8 : string) ]] in
  require_equal (module Sexp) (round_trip_through_elisp input) input;
  (* A valid-UTF-8 atom converts to multibyte text even alongside an atom holding
     non-UTF-8 bytes, and the sexp still round-trips. *)
  let mixed = [%sexp [ "✨"; (invalid_utf8 : string) ]] in
  require (multibyte_string_p (Value.Type.to_value Ecaml_sexp.t mixed |> car));
  require_equal (module Sexp) (round_trip_through_elisp mixed) mixed;
  (* So does an atom mixing UTF-8 text with non-UTF-8 bytes. *)
  let mixed_within_atom = [%sexp [ ("\xff sparkles \xe2\x9c\xa8" : string) ]] in
  require_equal
    (module Sexp)
    (round_trip_through_elisp mixed_within_atom)
    mixed_within_atom;
  return ()
;;

let%expect_test "[of_prin1_output]" =
  let test string = print_s (Ecaml_sexp.Private.of_prin1_output string) in
  (* Octal escapes, including ones shorter than three digits. *)
  test {|("\342\234\250 sparkles" "caf\303\251" "\40")|};
  [%expect {| ("\226\156\168 sparkles" "caf\195\169" " ") |}];
  (* Named escapes, and escaped characters that stand for themselves. *)
  test {|"tab\there quote\" backslash\\ paren\("|};
  [%expect {| "tab\there quote\" backslash\\ paren(" |}];
  (* [nil] is the only bare atom we accept; it is how [prin1] prints the empty list, so it
     parses as [()], at top level and nested. *)
  test {|nil|};
  [%expect {| () |}];
  test {|(nil "a" (nil))|};
  [%expect {| (() a (())) |}];
  (* Other bare atoms, such as symbols and numbers, are rejected, as are dotted pairs (the
     [.] is a bare atom). [to_form_elisp] produces only strings and lists, so [prin1]
     output for values [Ecaml_sexp] creates never contains them. *)
  require_does_raise (fun () -> test {|(foo)|});
  [%expect
    {|
    ("could not parse [prin1] output"
     "unsupported bare atom"
     (string "(foo)")
     (pos    4))
    |}];
  require_does_raise (fun () -> test {|42|});
  [%expect
    {|
    ("could not parse [prin1] output"
     "unsupported bare atom"
     (string 42)
     (pos    2))
    |}];
  require_does_raise (fun () -> test {|("a" . "b")|});
  [%expect
    {|
    ("could not parse [prin1] output"
     "unsupported bare atom"
     (string "(\"a\" . \"b\")")
     (pos    6))
    |}];
  require_does_raise (fun () -> test {|"\777"|});
  [%expect
    {|
    ("could not parse [prin1] output"
     "octal escape out of range"
     (string "\"\\777\"")
     (pos    5))
    |}];
  require_does_raise (fun () -> test {|"\x41"|});
  [%expect
    {|
    ("could not parse [prin1] output"
     "unsupported string escape"
     (string "\"\\x41\"")
     (pos    3))
    |}];
  require_does_raise (fun () -> test {|("a" "b") "c"|});
  [%expect
    {|
    ("could not parse [prin1] output"
     "trailing data"
     (string "(\"a\" \"b\") \"c\"")
     (pos    10))
    |}];
  require_does_raise (fun () -> test {|("unterminated|});
  [%expect
    {|
    ("could not parse [prin1] output"
     "unexpected end of input"
     (string "(\"unterminated")
     (pos    14))
    |}];
  return ()
;;

let read = Funcall.Wrap.("read" <: string @-> return value)

(* The sexp that parsing the [prin1] output of [value] should produce, computed directly
   from the Elisp value rather than by parsing. *)
let rec reference_sexp_of_value value : Sexp.t =
  if Value.is_nil value
  then List []
  else if Value.is_string value
  then Atom (Value.to_utf8_bytes_exn value)
  else if Value.is_cons value
  then (
    let rec loop (rev_forms : Sexp.t Reversed_list.t) value =
      if Value.is_nil value
      then Reversed_list.rev rev_forms
      else
        loop
          (reference_sexp_of_value (Value.car_exn value) :: rev_forms)
          (Value.cdr_exn value)
    in
    List (loop [] value))
  else
    raise_s
      [%message
        "unsupported Elisp value" ~_:(Value.prin1_to_string value : string) [%here]]
;;

(* Use Elisp's [read] as the reference implementation: [of_prin1_output] must interpret
   [prin1] output the same way [read] does. *)
let require_parse_agrees_with_read value =
  let prin1_output = Value.prin1_to_string value in
  let actual = Ecaml_sexp.Private.of_prin1_output prin1_output in
  let expect = reference_sexp_of_value (read prin1_output) in
  require_equal
    (module Sexp)
    actual
    expect
    ~message:"[of_prin1_output] disagrees with [read]"
    ~if_false_then_print_s:(lazy [%message (prin1_output : string)])
;;

let%expect_test "quickcheck: [of_prin1_output] vs [read] on values built from sexps" =
  (* This exercises the values [Ecaml_sexp] itself creates, including the unibyte strings
     that [to_form_elisp] produces for atoms with non-ASCII bytes. *)
  let%quick_test prop (sexp : Sexp.t) =
    require_parse_agrees_with_read (Ecaml_sexp.to_value sexp)
  in
  [%expect {| |}];
  return ()
;;

let%expect_test "Elisp values with non-string leaves are rejected" =
  (* The Elisp-to-OCaml conversion is only meant to be applied to values that started life
     as OCaml sexps, which contain only strings, proper lists, and [nil]. *)
  let%bind value = Form.eval_string {|'(foo 42 1.5 nil (a . b) "str")|} in
  require_does_raise ~hide_positions:true (fun () -> ecaml_sexp_pp_value_to_string value);
  [%expect
    {|
    ((
      "function got argument of wrong type"
      ecaml-sexp-pp-to-string
      (defined_at app/emacs/lib/ecaml/src/ecaml_sexp.ml:LINE:COL)
      (arg sexp)
      ("unable to convert Elisp value to OCaml value"
       (type_ sexp)
       (value (foo 42 1.5 nil (a . b) str))
       (exn (
         "could not parse [prin1] output"
         "unsupported bare atom"
         (string "(foo 42 1.5 nil (a . b) \"str\")")
         (pos    4))))))
    |}];
  let%bind value = Form.eval_string {|'(nil "str" ())|} in
  print_endline (ecaml_sexp_pp_value_to_string value);
  [%expect {| (() str ()) |}];
  return ()
;;

let%expect_test "empty lists round-trip" =
  let input = [%sexp ([] : int list), ([ [] ] : int list list), Some ()] in
  require_equal (module Sexp) (round_trip_through_elisp input) input;
  print_endline (ecaml_sexp_pp_to_string input);
  [%expect {| (() (()) (Some ())) |}];
  return ()
;;

let%expect_test "sexp with non-ASCII atoms" =
  (* The atoms are UTF-8; converting them to Elisp and back should preserve them. *)
  let input = [%sexp ([ "✨ sparkles"; "caf\xc3\xa9" ] : string list)] in
  require_equal (module Sexp) (round_trip_through_elisp input) input;
  (* Pretty-printing shows UTF-8 atoms verbatim. *)
  print_endline (ecaml_sexp_pp_to_string input);
  [%expect {| ("✨ sparkles" café) |}];
  (* Atoms that are not valid UTF-8 pretty-print escaped, and still round-trip. *)
  let input = [%sexp ([ "\xff\xfe binary" ] : string list)] in
  require_equal (module Sexp) (round_trip_through_elisp input) input;
  print_endline (ecaml_sexp_pp_to_string input);
  [%expect {| ("\255\254 binary") |}];
  return ()
;;

let%expect_test "sexp" =
  let foo, bar, leading_dot = "foo foo", "'bar", ".leading_dot" in
  let list = [%sexp (foo : string), (bar : string), (leading_dot : string)] in
  let input = [%sexp (list : Sexp.t), (list : Sexp.t)] in
  print_s input;
  [%expect
    {|
    (("foo foo" 'bar .leading_dot)
     ("foo foo" 'bar .leading_dot))
    |}];
  print_s (round_trip_through_elisp input);
  [%expect
    {|
    (("foo foo" 'bar .leading_dot)
     ("foo foo" 'bar .leading_dot))
    |}];
  print_endline (ecaml_sexp_pp_to_string input);
  [%expect
    {|
    (("foo foo" 'bar .leading_dot)
     ("foo foo" 'bar .leading_dot))
    |}];
  return ()
;;
