open! Core_kernel
open! Async_kernel
open! Import

module Q = struct
  (* An arbitrarily-chosen feature distributed with emacs, but not loaded on emacs startup. *)
  let array = "array" |> Symbol.intern
end

let%expect_test _ =
  print_s [%sexp (Feature.is_provided Q.array : bool)];
  [%expect {| false |}];
  Eval.after_load [%here] Q.array ~f:(fun () -> print_s [%sexp "Hello world"]);
  [%expect {||}];
  Feature.require Q.array;
  [%expect {| "Hello world" |}];
  Eval.after_load [%here] Q.array ~f:(fun () ->
    print_s [%sexp "This should print immediately"]);
  [%expect {| "This should print immediately" |}];
  return ()
;;
