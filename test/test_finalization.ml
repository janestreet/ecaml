open! Core_kernel
open! Import

let callback_from_emacs_to_ocaml =
  Function.to_value (Function.create [%here] ~args:[] (fun _ -> Value.nil))
;;

let make_ocaml_garbage_not_keep_emacs_values_alive () =
  (* make the ocaml gc run the finalizers in the Value.t custom blocks *)
  Gc.compact ();
  Gc.compact ();
  (* and make sure we call a wrapped ocaml function, which is where we actually call the
     emacs free function *)
  Value.funcall0_i callback_from_emacs_to_ocaml;
;;

let%expect_test "Emacs objects no longer referenced from OCaml can be gc'ed by Emacs" =
  make_ocaml_garbage_not_keep_emacs_values_alive ();
  let before = Value.Stat.now () in
  ignore (Value.cons Value.nil (Value.cons Value.nil Value.nil) : Value.t);
  make_ocaml_garbage_not_keep_emacs_values_alive ();
  let after = Value.Stat.now () in
  print_s [%sexp (Value.Stat.diff after before : Value.Stat.t)];
  [%expect "
    ((emacs_free_performed 2)
     (emacs_free_scheduled 2))"];
;;

let emacs_garbage_collect =
  let f = "garbage-collect" |> Symbol.intern in
  fun () -> Symbol.funcall0_i f
;;

let%expect_test "OCaml objects no longer referenced from Emacs can be gc'ed by OCaml" =
  let force_closure_allocation = Random.int 1232132 in
  let ocaml_value = (fun _ -> Gc.keep_alive force_closure_allocation; Value.nil) in
  let ocaml_value_is_alive =
    let w = Weak_pointer.create () in
    Weak_pointer.set w (Heap_block.create_exn ocaml_value);
    fun () -> Weak_pointer.is_some w
  in
  ignore (Function.to_value (Function.create [%here] ~args:[] ocaml_value) : Value.t);
  make_ocaml_garbage_not_keep_emacs_values_alive ();
  print_s [%sexp (ocaml_value_is_alive () : bool)];
  [%expect "true"];
  (* We now make Emacs tell us the function id is garbage, at which point we remove the
     OCaml function from the registration table. *)
  emacs_garbage_collect ();
  Gc.compact (); (* and get the OCaml gc to realize the function is gone *)
  print_s [%sexp (ocaml_value_is_alive () : bool)];
  [%expect "false"];
;;

let%expect_test "finalization of an Emacs function" =
  let r = ref 14 in
  let f _ = r := 14; Value.nil in
  Gc.Expert.add_finalizer_exn f
    (fun _ -> print_s [%message "finalized" (r : int ref)]);
  ignore (Function.create [%here] ~args:[] f : Function.t);
  make_ocaml_garbage_not_keep_emacs_values_alive ();
  emacs_garbage_collect ();
  Gc.compact ();
  [%expect {| (finalized (r 14)) |}]
;;
