open! Core_kernel
open! Async_kernel
open! Import

let find_function =
  let f = Funcall.Wrap.("find-function" <: Symbol.t @-> return nil) in
  fun symbol -> Async_ecaml.Private.run_outside_async [%here] (fun () -> f symbol)
;;

let find_ocaml ~library ~symbol ~type_ =
  let%map buffer = Buffer.find_file_noselect library in
  ( buffer
  , Some ((Load_history.location_exn symbol type_).pos_cnum + 1 |> Position.of_int_exn) )
;;

let advise_for_ocaml () =
  Feature.require ("find-func" |> Symbol.intern);
  let for_function = "find-function-search-for-symbol" |> Symbol.intern in
  Advice.around_values
    ("find-function-search-ocaml" |> Symbol.intern)
    [%here]
    ~for_function
    Async
    (fun inner values ->
       let buffer_and_position =
         match values with
         | [ symbol; type_; library ] ->
           let do_it () =
             let library = Value.to_utf8_bytes_exn library in
             if not (String.is_suffix library ~suffix:".ml")
             then return None
             else (
               (* [find-function-search-for-symbol] is used by both [find-function] and
                  [find-variable], so [symbol] can be a function or a variable. *)
               let symbol = Symbol.of_value_exn symbol in
               let type_ = Load_history.Type.of_value_exn type_ in
               let%map result = find_ocaml ~library ~symbol ~type_ in
               let type_ = Value.Type.(tuple Buffer.t (nil_or Position.t)) in
               Some (Value.Type.to_value type_ result))
           in
           (match%map Monitor.try_with do_it with
            | Ok result -> result
            | Error _ -> None)
         | _ -> return None
       in
       match%map buffer_and_position with
       | None -> inner values
       | Some x -> x)
;;

let () = advise_for_ocaml ()
