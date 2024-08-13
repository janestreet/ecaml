open! Core
open! Async_kernel
open! Import

let identity = Funcall.Wrap.("identity" <: Ecaml_sexp.t @-> return Ecaml_sexp.t)

let ecaml_sexp_pp_to_string =
  Funcall.Wrap.("ecaml-sexp-pp-to-string" <: Ecaml_sexp.t @-> return string)
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
  print_s (identity input);
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
