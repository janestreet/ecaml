open! Core
open! Import
module Scheduler = Async_unix.Async_unix_private.Raw_scheduler

let scheduler = Scheduler.t ()

module Arity = struct
  type 'callback t = Arity1 : ('a1 -> 'r) t [@@deriving sexp_of]
end

open Arity

type 'callback t =
  { arity : 'callback Arity.t
  ; name : string
  }
[@@deriving sexp_of]

let report_exn_when_calling_callback =
  let out_of_memory_message = "Ecaml received Out_of_memory" in
  let out_of_memory_value = out_of_memory_message |> Value.of_utf8_bytes in
  function
  | Out_of_memory ->
    (try Value.Private.message_zero_alloc out_of_memory_value with
     | _ -> eprintf "%s" out_of_memory_message)
  | exn ->
    let sexp = [%message "Ecaml callback handling raised" (exn : Exn.t)] in
    (try Value.message_s sexp with
     | _ -> eprint_s sexp)
;;

let registered_callbacks : Source_code_position.t String.Table.t = String.Table.create ()

let register
  (type callback)
  ~(here : [%call_pos])
  (t : callback t)
  ~(f : callback)
  ~should_run_holding_async_lock
  =
  (match Hashtbl.find registered_callbacks t.name with
   | Some already_registered_at ->
     raise_s
       [%sexp
         "Multiple registrations for ecaml callback"
         , { name : string = t.name
           ; already_registered_at : Source_code_position.t
           ; repeat_registration_at : Source_code_position.t = here
           }]
   | None -> Hashtbl.set registered_callbacks ~key:t.name ~data:here);
  let with_lock f =
    if Scheduler.am_holding_lock scheduler then f () else Scheduler.with_lock scheduler f
  in
  let callback : callback =
    match t.arity with
    | Arity1 ->
      fun a1 ->
        (try
           if not should_run_holding_async_lock then f a1 else with_lock (fun () -> f a1)
         with
         | exn ->
           report_exn_when_calling_callback exn;
           raise exn)
  in
  (Stdlib.Callback.register [@ocaml.alert "-unsafe_multidomain"]) t.name callback
;;

let on_end_of_module_initialization =
  let functions = ref [] in
  let () =
    register
      { arity = Arity1; name = "end_of_module_initialization" }
      ~should_run_holding_async_lock:false
      ~f:(fun () ->
        List.iter !functions ~f:(fun (here, f) ->
          try f `Not_holding_the_async_lock with
          | exn ->
            eprint_s
              [%message
                "on_end_of_module_initialization function raised"
                  ~_:(here : Source_code_position.t)
                  (exn : exn)
                  ~backtrace:(Backtrace.get () : Backtrace.t)]))
  in
  fun ~(here : [%call_pos]) f -> functions := (here, f) :: !functions
;;

(** [no_active_env] is used when the C code detects that OCaml is attempting to call an
    Emacs function but there is no active env. It prints a message that includes an OCaml
    backtrace, which may be useful in debugging. *)
let () =
  register
    { arity = Arity1; name = "no_active_env" }
    ~f:(fun () ->
      eprint_s
        [%message
          "Ecaml called with no active env" ~backtrace:(Backtrace.get () : Backtrace.t)])
    ~should_run_holding_async_lock:true
;;

let free_embedded_caml_values = { arity = Arity1; name = "free_embedded_caml_values" }
