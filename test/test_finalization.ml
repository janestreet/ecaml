open! Core
open! Async_kernel
open! Import

(* When Emacs is no longer holding onto an embedded OCaml value, it is freed by C function
   [free_embedded_caml_values], which is called only in [acquire_ocaml_lock_from_emacs],
   which in turn is only called in [Fdispatch]. The latter function is used when Emacs
   wants to call into OCaml, for example to invoke an Elisp function whose underlying
   implementation is an OCaml function. *)
let run_in_a_different_environment_staged f =
  let lambda = lambda_nullary_nil [%here] f in
  stage (fun () -> Function.to_value lambda |> Value.funcall0_i)
;;

(* This function is staged so that we only allocate the lambda once. *)
let free_embedded_ocaml_values =
  unstage (run_in_a_different_environment_staged (fun () -> ()))
;;

let run_in_a_different_environment f =
  unstage (run_in_a_different_environment_staged f) ()
;;

let make_ocaml_garbage_not_keep_emacs_values_alive () =
  (* make the ocaml gc run the finalizers in the Value.t custom blocks *)
  Gc.compact ();
  Gc.compact ();
  (* and make sure we call a wrapped ocaml function, which is where we actually call the
     emacs free function *)
  free_embedded_ocaml_values ()
;;

let emacs_garbage_collect =
  let garbage_collect = Funcall.Wrap.("garbage-collect" <: nullary @-> return nil) in
  fun () ->
    garbage_collect ();
    (* Remove reference to OCaml values that are no longer referenced by emacs. We need to
       go into dispatch for that to happen so we just need to call a function here *)
    free_embedded_ocaml_values ()
;;

let%expect_test "finalization of embedded ocaml values" =
  let dump_embedded_values () =
    (* Each call to [defun] puts an entry in this table, which is just noise here. *)
    Ecaml.debug_embedded_caml_values ()
    |> [%of_sexp: (int * Sexp.t) list]
    |> List.filter_map ~f:(fun (_, sexp) ->
      match sexp with
      | Atom "<fun>" -> None
      | _ -> Some sexp)
    |> [%sexp_of: Sexp.t list]
    |> print_s
  in
  dump_embedded_values ();
  run_in_a_different_environment (fun () ->
    [%expect {| () |}];
    let module A = struct
      type t =
        { a : string
        ; b : int
        }
      [@@deriving sexp_of]

      let type_id = Type_equal.Id.create ~name:"A" sexp_of_t
    end
    in
    let a_type = Caml_embed.create_type A.type_id in
    let var_name = "embedded-var-a" in
    let var = Var.create (Symbol.create_uninterned ~name:var_name) a_type in
    Var.make_buffer_local_always var;
    let v1 : A.t = { a = "V1"; b = 1 } in
    let v2 : A.t = { a = "V2"; b = 2 } in
    Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
      Current_buffer.set_value var v1;
      Current_buffer.set_value var v2;
      dump_embedded_values ());
    [%expect
      {|
      (((a V1) (b 1))
       ((a V2) (b 2)))
      |}]);
  make_ocaml_garbage_not_keep_emacs_values_alive ();
  emacs_garbage_collect ();
  make_ocaml_garbage_not_keep_emacs_values_alive ();
  dump_embedded_values ();
  [%expect {| () |}];
  return ()
;;

let%expect_test "Emacs objects no longer referenced from OCaml can be gc'ed by Emacs" =
  make_ocaml_garbage_not_keep_emacs_values_alive ();
  let before = Value.Stat.now () in
  ignore (Value.cons Value.nil (Value.cons Value.nil Value.nil) : Value.t);
  make_ocaml_garbage_not_keep_emacs_values_alive ();
  let after = Value.Stat.now () in
  print_s [%sexp (Value.Stat.diff after before : Value.Stat.t)];
  [%expect
    {|
    ((free_performed 6)
     (free_scheduled 6)
     (root_allocated 6))
    |}];
  return ()
;;

let test_ocaml_gc_handles_references_from_emacs ~make_emacs_reference =
  let force_closure_allocation = Random.int 1232132 in
  let ocaml_value _ =
    Gc.keep_alive force_closure_allocation;
    Value.nil
  in
  let ocaml_value_is_alive =
    let w = Weak_pointer.create () in
    Weak_pointer.set w (Heap_block.create_exn ocaml_value);
    fun () -> Weak_pointer.is_some w
  in
  ignore (make_emacs_reference ~ocaml_value : Value.t);
  (* When built with OCaml 4.08 or later, this test becomes flaky. Sometimes, the Emacs GC
     doesn't garbage collect the above emacs reference, and so [alive_after = true]. We
     don't understand why this is, but allocating a string of length >= 16 makes the test
     succeed deterministically. *)
  ignore (Value.of_utf8_bytes "aaaaaaaaaaaaaaaa" : Value.t);
  let alive_before = ocaml_value_is_alive () in
  make_ocaml_garbage_not_keep_emacs_values_alive ();
  (* We now make Emacs tell us the function id is garbage, at which point we remove the
     OCaml function from the registration table. *)
  emacs_garbage_collect ();
  Gc.compact ();
  (* and get the OCaml gc to realize the function is gone *)
  let alive_after = ocaml_value_is_alive () in
  print_s [%message (alive_before : bool) (alive_after : bool)]
;;

let%expect_test ("OCaml objects no longer referenced from Emacs can be gc'ed by OCaml"
  [@tags "disabled"])
  =
  test_ocaml_gc_handles_references_from_emacs ~make_emacs_reference:(fun ~ocaml_value ->
    Function.of_ocaml_func0 [%here] ocaml_value |> Function.to_value);
  [%expect
    {|
    ((alive_before true)
     (alive_after  false))
    |}];
  test_ocaml_gc_handles_references_from_emacs ~make_emacs_reference:(fun ~ocaml_value ->
    lambda_nullary [%here] (Returns Value.Type.value) ocaml_value |> Function.to_value);
  [%expect
    {|
    ((alive_before true)
     (alive_after  false))
    |}];
  return ()
;;

let%expect_test "finalization of an Emacs function" =
  run_in_a_different_environment (fun () ->
    let r = ref 14 in
    let f _ =
      r := 14;
      Value.nil
    in
    Core.Gc.Expert.add_finalizer_ignore f (fun _ ->
      print_s [%message "finalized" (r : int ref)]);
    ignore (Function.of_ocaml_func0 [%here] f : Function.t));
  make_ocaml_garbage_not_keep_emacs_values_alive ();
  emacs_garbage_collect ();
  Gc.compact ();
  [%expect {| (finalized (r 14)) |}];
  return ()
;;
