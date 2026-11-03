open! Core
open! Async
open! Import
include (val Symbol_prefix.extend symbol_prefix "deferred")

type request =
  | Request :
      { deferred : 'a Deferred.Or_error.t
      ; result_type : 'a Value.Type.t
      }
      -> request

let sexp_of_request (_ : request) = Sexp.Atom "<ecaml-deferred>"

let request_type =
  Caml_embed.create_type
    (Type_equal.Id.create ~name:"Ecaml.Deferred_value.request" sexp_of_request)
;;

let type_ (type a) (result_type : a Value.Type.t) =
  Value.Type.create
    [%message "Deferred_value.t" ~result_type:(Value.Type.name result_type : Sexp.t)]
    (fun deferred ->
      match Deferred.peek deferred with
      | None -> Sexp.Atom "<pending>"
      | Some result -> Or_error.sexp_of_t (Value.Type.to_sexp result_type) result)
    (fun value ->
      (* Note that this of_value function mostly isn't used; finished-p and wait both use
         [request_type] *)
      let (Request { deferred; result_type = actual_result_type }) =
        Value.Type.of_value_exn request_type value
      in
      match
        Type_equal.Id.same_witness
          (Value.Type.id actual_result_type)
          (Value.Type.id result_type)
      with
      | Some T -> deferred
      | None ->
        raise_s
          [%message
            "Deferred result type mismatch"
              ~expected:(Value.Type.name result_type : Sexp.t)
              ~actual:(Value.Type.name actual_result_type : Sexp.t)])
    (fun deferred -> Value.Type.to_value request_type (Request { deferred; result_type }))
;;

let () =
  Defun.defun
    ("finished-p" |> symbol)
    [%here]
    ~docstring:"Return non-nil if REQUEST has finished."
    (Returns Value.Type.bool)
    (let%map_open.Defun (Request { deferred; _ }) = required "request" request_type in
     Deferred.is_determined deferred);
  Defun.defun
    ("wait" |> symbol)
    [%here]
    ~docstring:"Wait for REQUEST to finish and return its value."
    (Returns Value.Type.value)
    (let%map_open.Defun (Request { deferred; result_type }) =
       required "request" request_type
     in
     let result =
       Async_ecaml.Private.block_on_async
         ~context:[%lazy_message "ecaml-deferred-wait" (result_type : _ Value.Type.t)]
         (fun () ->
           Monitor.try_with ~extract_exn:true (fun () ->
             let%map result = deferred in
             Or_error.ok_exn result))
       |> Result.ok_exn
     in
     Value.Type.to_value result_type result)
;;
