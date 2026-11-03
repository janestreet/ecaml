open! Core
open! Async
open! Import

let finished_p = Funcall.Wrap.("ecaml-deferred-finished-p" <: value @-> return bool)
let wait = Funcall.Wrap.("ecaml-deferred-wait" <: value @-> return value)

let to_elisp result_type request =
  Value.Type.to_value (Deferred_value.type_ result_type) request
;;

let wait_outside_async request =
  Async_ecaml.Private.run_outside_async (fun () -> wait request)
;;

(* Censor the addresses in the printed representation of a user-ptr, e.g.
   [#<user-ptr ptr=0x1df1b810 finalizer=0x7f760f5bbd00>], which vary between runs. *)
let sanitize_user_ptrs sexp =
  smash_sexp sexp ~f:(function
    | Sexp.Atom atom when String.is_prefix atom ~prefix:"#<user-ptr " ->
      Atom
        (Regexp.replace_string (Regexp.of_pattern "0x[0-9a-f]+") ~with_:"ADDR" ~in_:atom)
    | sexp -> sexp)
;;

let%expect_test "Deferred sexps use the result type" =
  let ivar = Ivar.create () in
  let deferred =
    let%map value = Ivar.read ivar in
    Ok value
  in
  let type_ = Deferred_value.type_ Value.Type.string in
  print_s (Value.Type.to_sexp type_ deferred);
  [%expect {| <pending> |}];
  Ivar.fill_exn ivar "finished";
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  print_s (Value.Type.to_sexp type_ deferred);
  [%expect {| (Ok finished) |}];
  return ()
;;

let%expect_test "Deferred values can be read back with the same result type" =
  let string_type = Deferred_value.type_ Value.Type.string in
  let deferred = Deferred.Or_error.return "finished" in
  let value = Value.Type.to_value string_type deferred in
  let round_trip = Value.Type.of_value_exn string_type value in
  print_s [%sexp (Deferred.peek round_trip : string Or_error.t option)];
  [%expect {| ((Ok finished)) |}];
  require_does_raise ~sanitize:sanitize_user_ptrs (fun () ->
    Value.Type.of_value_exn (Deferred_value.type_ Value.Type.int) value);
  [%expect
    {|
    ("unable to convert Elisp value to OCaml value"
     (type_ (Deferred_value.t (result_type int)))
     (value "#<user-ptr ptr=ADDR finalizer=ADDR>")
     (exn (
       "Deferred result type mismatch"
       (expected int)
       (actual   string))))
    |}];
  return ()
;;

let%expect_test "pending and finished Deferred values" =
  let ivar = Ivar.create () in
  let request =
    (let%map value = Ivar.read ivar in
     Ok value)
    |> to_elisp Value.Type.string
  in
  print_s [%sexp (finished_p request : bool)];
  [%expect {| false |}];
  Ivar.fill_exn ivar "finished value";
  let%map value = wait_outside_async request in
  print_s [%sexp (finished_p request : bool)];
  [%expect {| true |}];
  print_s [%sexp (Value.Type.of_value_exn Value.Type.string value : string)];
  [%expect {| "finished value" |}]
;;

let%expect_test "serialization is delayed until wait" =
  let result_type =
    Value.Type.create
      [%message "counted string"]
      [%sexp_of: string]
      Value.to_utf8_bytes_exn
      (fun string ->
         print_endline "serializer called!";
         Value.of_utf8_bytes string)
  in
  let request = Deferred.Or_error.return "finished value" |> to_elisp result_type in
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  (* The request has finished but the serializer isn't called until the value is waited
     upon. *)
  require (finished_p request);
  [%expect {| |}];
  let%map value = wait_outside_async request in
  print_s [%sexp (Value.Type.of_value_exn Value.Type.string value : string)];
  [%expect
    {|
    serializer called!
    "finished value"
    |}]
;;

let%expect_test "Deferred errors are raised by wait" =
  let request = Deferred.Or_error.error_string "failed" |> to_elisp Value.Type.string in
  let%bind wait_result =
    Monitor.try_with_or_error (fun () -> wait_outside_async request)
  in
  require (finished_p request);
  require_error ~print_error:true [%sexp_of: Value.t] wait_result;
  [%expect
    {|
    (monitor.ml.Error
      (failed)
      ("<backtrace elided in test>" "Caught by monitor try_with_or_error"))
    |}];
  return ()
;;

let%expect_test "serialization exceptions are raised by wait" =
  let result_type =
    Value.Type.create
      [%message "raising string"]
      [%sexp_of: string]
      Value.to_utf8_bytes_exn
      (fun (_ : string) -> raise_s [%message "serialization failed"])
  in
  let request = Deferred.Or_error.return "finished" |> to_elisp result_type in
  let%bind wait_result =
    Monitor.try_with_or_error (fun () -> wait_outside_async request)
  in
  require (finished_p request);
  require_error ~print_error:true [%sexp_of: Value.t] wait_result;
  [%expect
    {|
    (monitor.ml.Error
      ("serialization failed")
      ("<backtrace elided in test>" "Caught by monitor try_with_or_error"))
    |}];
  return ()
;;
