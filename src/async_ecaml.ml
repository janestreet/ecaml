open! Core
open! Async
open! Import

module Condition = Core.Condition
module Unix      = Core.Unix

module File_descr = Unix.File_descr

module Scheduler = Async_unix.Raw_scheduler

type t =
  { mutable have_requested_cycle : bool
  ; ran_cycle                    : Condition.t sexp_opaque
  ; ran_cycle_mutex              : Mutex.t sexp_opaque
  ; scheduler                    : Scheduler.t
  ; write_to_request_cycle       : File_descr.t
  }
[@@deriving sexp_of]

let run_async_cycle t =
  if debug
  then (Debug.eprint_s [%message "running a cycle" ~time:(Time.now () : Time.t)]);
  Async_kernel_private.Scheduler.(run_cycle (t ()));
  t.have_requested_cycle <- false;
  Mutex.critical_section t.ran_cycle_mutex ~f:(fun () ->
    Condition.broadcast t.ran_cycle)
;;

let have_lock_do_cycle =
  let module M = struct
    external have_active_env : unit -> bool = "ecaml_have_active_env"
  end in
  fun t ->
    (* We must be able to run a cycle even if an Emacs callback is blocked on a deferred
       inside [Thread_safe.block_on_async].  Since Emacs callbacks always have an active
       env, we can allow other threads (e.g. the Async scheduler) that respect Async
       locking to run a cycle if they if there is an active env.  As part of
       [Thread_safe.block_on_async], the blocked callback will release and re-acquire the
       Async lock while waiting on the deferred.  But it will otherwise hold the Async
       lock, which will, as usual prevent multiple threads from simultaneously doing Async
       things. *)
    if M.have_active_env ()
    then (run_async_cycle t)
    else if not t.have_requested_cycle
    then (
      t.have_requested_cycle <- true;
      Scheduler.unlock t.scheduler;
      ignore (Unix.single_write t.write_to_request_cycle ~buf:"\x05" : int);
      Mutex.critical_section t.ran_cycle_mutex ~f:(fun () ->
        Condition.wait t.ran_cycle t.ran_cycle_mutex);
      Scheduler.lock t.scheduler);
;;

let create () =
  let scheduler = Scheduler.t () in
  ignore (Thread.create Scheduler.go () : Thread.t);
  (* We release the lock only at the end of module initialization, to avoid racing with
     the scheduler, which is running in another thread. *)
  Ecaml_callback.(register end_of_module_initialization)
    ~should_run_holding_async_lock:false
    ~f:(fun () -> Scheduler.(unlock scheduler));
  let write_to_request_cycle =
    Unix.socket ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0 in
  let t =
    { have_requested_cycle   = false
    ; ran_cycle              = Condition.create ()
    ; ran_cycle_mutex        = Mutex.create ()
    ; scheduler
    ; write_to_request_cycle
    }
  in
  t.scheduler.have_lock_do_cycle <- Some (fun () -> have_lock_do_cycle t);
  (* This reduces load on emacs to a negligible level. *)
  Scheduler.set_max_inter_cycle_timeout (sec 1.);
  t
;;

let connect t ~socket_path =
  Unix.connect t.write_to_request_cycle ~addr:(ADDR_UNIX socket_path)
;;

let initialize_module =
  if debug
  then (Debug.eprint_s [%message "initializing async" [%here] (Time.now () : Time.t)]);
  let socket_path = ".ecaml." ^ (Unix.getpid () |> Pid.to_string) in
  let t = create () in
  let run_async_cycle =
    Function.create
      ~docstring:"runs an async cycle"
      ~args:[]
      (fun _ -> run_async_cycle t; Value.nil)
  in
  let intern = Value.intern in
  Symbol.(funcall_i (intern "make-network-process"))
    [ intern ":name"    ; "async scheduler" |> Value.of_string
    ; intern ":family"  ; "local"           |> intern
    ; intern ":server"  ; Symbol.t          |> Symbol.to_value
    ; intern ":service" ; socket_path       |> Value.of_string
    ; intern ":filter"  ; run_async_cycle   |> Function.to_value ];
  connect t ~socket_path; (* must be before [unlink]ing *)
  Unix.unlink socket_path;
;;

