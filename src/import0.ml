open! Core
module Caml_embed = Ecaml_value.Caml_embed
module Ecaml_callback = Ecaml_value.Ecaml_callback
module Form = Ecaml_value.Form
module Funcall = Ecaml_value.Funcall
module Function = Ecaml_value.Function
module Value = Ecaml_value.Value
module Valueable = Ecaml_value.Valueable

let message = Ecaml_value.message
let messagef = Ecaml_value.messagef
let message_s = Ecaml_value.message_s

include Composition_infix

let () =
  Printexc.record_backtrace true;
  Dynamic.set_root Sexp.of_int_style `Underscores
;;

let concat = String.concat
let debug = false
let print_s = Expect_test_helpers_core.print_s
let eprint_s = Core.Debug.eprint_s
let raise_string s = raise_s [%sexp (String.strip (concat s) : string)]
let raise_user_error s = raise (Value.Expert.User_error (String.strip s))
let sec_ns = Time_ns.Span.of_sec

type 'a opaque_in_test = 'a

let sexp_of_opaque_in_test sexp_of_a a =
  if am_running_test then [%sexp "_"] else [%sexp (a : a)]
;;

let require_nonempty_docstring here ~docstring =
  if String.is_empty docstring
  then
    raise_s
      [%message "empty docstrings are not allowed" ~_:(here : Source_code_position.t)]
;;
