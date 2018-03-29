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

(* Buffer local variables can cause emacs to segfault when gc'ed. So we use these
   functions to prevent such variables from being gc'ed. See thread "nasty ecaml bug" or
   bug report http://lists.gnu.org/archive/html/bug-gnu-emacs/2017-10/msg01453.html.
   This workaround can cause a memory leak in theory, but in practice, they are used very
   little. *)
let add_gc_root =
  let gc_roots = Stack.create () in
  fun f -> Stack.push gc_roots f
let add_gc_root a = add_gc_root (fun () -> Gc.keep_alive a)

(* included last so it can't be shadowed *)
include Int.Replace_polymorphic_compare
