open! Core
open! Async.Std

let initialize_module =
  Printexc.record_backtrace true;
  Sexp.of_int_style := `Underscores;
;;

let debug = false

let eprint_s = Core.Debug.eprint_s
