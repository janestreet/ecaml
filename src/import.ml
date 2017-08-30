open! Core_kernel

let initialize_module =
  Printexc.record_backtrace true;
  Sexp.of_int_style := `Underscores;
;;

let concat = String.concat

let debug = false

let print_s = Expect_test_helpers_kernel.print_s

let eprint_s = Core_kernel.Debug.eprint_s

let async_ecaml_is_enabled = false
