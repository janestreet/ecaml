open! Core
open! Async
open! Import

module Value = Value0

module Scheduler = Async_unix.Raw_scheduler

let scheduler = Scheduler.t ()

module Arity = struct
  type 'callback t =
    | Arity1 : ('a1 ->        'r) t
    | Arity2 : ('a1 -> 'a2 -> 'r) t
  [@@deriving sexp_of]
end

open Arity

type 'callback t =
  { arity : 'callback Arity.t
  ; name  : string
  }
[@@deriving sexp_of]

let register (type callback)
      (t : callback t)
      ~(f : callback)
      ~should_run_holding_async_lock =
  let with_lock f =
    if Scheduler.am_holding_lock scheduler
    then (f ())
    else (Scheduler.with_lock scheduler f)
  in
  let callback =
    if not should_run_holding_async_lock
    then f
    else (
      match t.arity with
      | Arity1 -> (fun a1    -> with_lock (fun () -> f a1))
      | Arity2 -> (fun a1 a2 -> with_lock (fun () -> f a1 a2)))
  in
  Caml.Callback.register t.name callback
;;

let attach_async_finalizer       = { arity = Arity1; name = "attach_async_finalizer"       }
let dispatch_function            = { arity = Arity2; name = "dispatch_function"            }
let end_of_module_initialization = { arity = Arity1; name = "end_of_module_initialization" }
let free_function                = { arity = Arity1; name = "free_function"                }
let no_active_env                = { arity = Arity1; name = "no_active_env"                }
