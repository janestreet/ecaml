(* This module adds support for running Async code in Ecaml.

   We change the way the scheduler works. The Async scheduler runs in its own thread, but
   all Async cycles now run within the main emacs thread. When the scheduler would run a
   cycle, it instead sends a packet to a socket that the emacs main thread listens to
   (which notifies emacs that it should run a cycle), and waits for the cycle to be run.
   This way, whenever we run Ecaml code, we have both the Emacs [active_env] and the Async
   lock. This way, it is always safe to call Ecaml functions from Async, and to modify
   Async data structures from Ecaml. *)

open! Core
open! Import
module Scheduler = Async_unix.Async_unix_private.Raw_scheduler

module Q = struct
  let ecaml_async_take_lock_do_cycle = "ecaml-async-take-lock-do-cycle" |> Symbol.intern
end

module Scheduler_status = struct
  type t =
    | Uninitialized
    | Running
    | Stopped
  [@@deriving sexp]

  let status = ref Uninitialized
end

module Cycle_report = struct
  let cycles : Time_ns.Span.t list ref = ref []
  let measuring = ref false
  let report_cycle time = if !measuring then cycles := time :: !cycles

  let generate_report () =
    let open Async in
    Echo_area.message "Collecting 10 seconds of cycle data...";
    measuring := true;
    let%map () = Clock_ns.after (Time_ns.Span.of_sec 10.) in
    measuring := false;
    let samples = !cycles in
    cycles := [];
    let buffer = Buffer.find_or_create ~name:"cycle report" in
    Selected_window.switch_to_buffer buffer;
    List.iter samples ~f:(fun sample ->
      Point.insert (sprintf !"%{Time_ns.Span}\n" sample));
    let avg =
      let sum = List.fold_left ~init:Time_ns.Span.zero ~f:Time_ns.Span.( + ) samples in
      Time_ns.Span.(sum / Float.of_int (List.length samples))
    in
    Point.insert (sprintf !"Average cycle time: %{Time_ns.Span}\n" avg)
  ;;
end

module Thread_safe_sleeper : sig
  type t [@@deriving sexp_of]

  val create : unit -> t
  val blocking_sleep : t -> unit
  val wake_up : t -> unit
end = struct
  type t =
    { mutex : Mutex.t sexp_opaque
    ; wake_up : Condition.t sexp_opaque
    }
  [@@deriving sexp_of]

  let create () = { mutex = Mutex.create (); wake_up = Condition.create () }
  let critical_section t ~f = Mutex.critical_section t.mutex ~f

  let blocking_sleep t =
    critical_section t ~f:(fun () -> Condition.wait t.wake_up t.mutex)
  ;;

  let wake_up t = critical_section t ~f:(fun () -> Condition.broadcast t.wake_up)
end

module Cycle_requester = struct
  type t =
    { mutable client_process : Process.t option
    ; mutable server_process : Process.t option
    ; write_to_request_cycle : Unix.File_descr.t
    }

  let create () =
    let write_to_request_cycle =
      Unix.socket ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0
    in
    { client_process = None; server_process = None; write_to_request_cycle }
  ;;

  let request_cycle t =
    try
      ignore
        (Unix.single_write t.write_to_request_cycle ~buf:(Bytes.of_string "\x05") : int)
    with
    | _ ->
      (* We ignore exceptions here, because the network socket may be closed, e.g. when
         Emacs is shutting down, and we don't want to fail in that case. *)
      ()
  ;;

  let register_cycle_handler t run_cycle =
    let socket_path = ".ecaml." ^ (Unix.getpid () |> Pid.to_string) in
    let server_process =
      Process.create_unix_network_process
        ()
        ~name:"Async scheduler"
        ~socket_path
        ~filter:(fun client_process _ ->
          (match t.client_process with
           | Some _ -> ()
           | None ->
             t.client_process <- Some client_process;
             Process.set_query_on_exit client_process false);
          run_cycle ())
    in
    Process.set_query_on_exit server_process false;
    Unix.connect t.write_to_request_cycle ~addr:(ADDR_UNIX socket_path);
    Unix.unlink socket_path;
    t.server_process <- Some server_process
  ;;

  let shutdown t =
    Option.iter t.client_process ~f:Process.kill;
    Option.iter t.server_process ~f:Process.kill
  ;;
end

type t =
  { cycle_done_sleeper : Thread_safe_sleeper.t
  ; cycle_requester : Cycle_requester.t
  ; emacs_thread_id : int
  ; mutable exceptions_raised_outside_emacs_env : exn list
  ; mutable have_requested_cycle : bool
  ; mutable keepalive_timer : Timer.t option
  ; mutable last_cycle_finished_at : Time_ns.t
  ; scheduler : Scheduler.t
  }

let t =
  { cycle_done_sleeper = Thread_safe_sleeper.create ()
  ; cycle_requester = Cycle_requester.create ()
  ; emacs_thread_id = Thread.(id (self ()))
  ; exceptions_raised_outside_emacs_env = []
  ; have_requested_cycle = false
  ; keepalive_timer = None
  ; last_cycle_finished_at = Time_ns.epoch
  ; scheduler = Scheduler.t ()
  }
;;

external have_active_env : unit -> bool = "ecaml_have_active_env"

let in_emacs_have_lock_do_cycle () =
  if not (have_active_env ()) || not (Scheduler.am_holding_lock t.scheduler)
  then raise_s [%sexp "[in_emacs_have_lock_do_cycle] should only be called by emacs"];
  if debug then Debug.eprint_s [%message "running a cycle" ~time:(Time.now () : Time.t)];
  List.iter t.exceptions_raised_outside_emacs_env ~f:(fun exn ->
    Echo_area.message_s [%sexp (exn : exn)]);
  t.exceptions_raised_outside_emacs_env <- [];
  let time = Time.now () in
  Async_kernel.Async_kernel_scheduler.(run_cycle (t ()));
  t.have_requested_cycle <- false;
  t.last_cycle_finished_at <- Time_ns.now ();
  if debug
  then
    Debug.eprint_s
      [%message "cycle took" ~time:(Time.diff (Time.now ()) time : Time.Span.t)];
  Thread_safe_sleeper.wake_up t.cycle_done_sleeper
;;

(* [request_emacs_run_cycle] requests the emacs main thread to run a cycle. It hands over
   the Async lock in the process. *)
let request_emacs_run_cycle () =
  if not t.have_requested_cycle
  then (
    let start = Time_ns.now () in
    t.have_requested_cycle <- true;
    Cycle_requester.request_cycle t.cycle_requester;
    Scheduler.unlock t.scheduler;
    Thread_safe_sleeper.blocking_sleep t.cycle_done_sleeper;
    Scheduler.lock t.scheduler;
    let diff = Time_ns.diff (Time_ns.now ()) start in
    Cycle_report.report_cycle diff)
;;

let lock_async_during_module_initialization () =
  (* Acquire the Async lock, releasing it once module initialization is done. *)
  Ecaml_callback.(register end_of_module_initialization)
    ~should_run_holding_async_lock:false
    ~f:(fun () -> Scheduler.unlock t.scheduler)
;;

let max_inter_cycle_timeout = Time_ns.Span.second

let start_scheduler () =
  match !Scheduler_status.status with
  | Stopped -> raise_s [%sexp "Async has been shut down and cannot be restarted"]
  | Running -> ()
  | Uninitialized ->
    Scheduler_status.status := Running;
    if debug
    then Debug.eprint_s [%message "initializing async" [%here] (Time.now () : Time.t)];
    ignore
      ( Thread.create
          (fun () ->
             match Scheduler.go () ~raise_unhandled_exn:true with
             | _ -> .
             | exception exn ->
               (match !Scheduler_status.status with
                (* If we requested the scheduler to stop, this exception is expected. *)
                | Stopped -> ()
                | Running | Uninitialized -> raise exn))
          ()
        : Thread.t );
    Defun.defun
      [%here]
      Value.Type.unit
      Q.ecaml_async_take_lock_do_cycle
      ~interactive:""
      (let open Defun.Let_syntax in
       let%map_open () = return () in
       in_emacs_have_lock_do_cycle ());
    Cycle_requester.register_cycle_handler t.cycle_requester in_emacs_have_lock_do_cycle;
    (* It is possible that emacs doesn't respond to a cycle request (maybe because emacs
       is under high load). Instead of letting the scheduler block forever on a cycle that
       will never run, we add a timer to ensure we run a cycle once per
       [max_inter_cycle_timeout]. *)
    t.keepalive_timer
    <- Some
         (Timer.run_after
            max_inter_cycle_timeout
            ~repeat:max_inter_cycle_timeout
            (fun () ->
               if Time_ns.Span.( >= )
                    (Time_ns.diff (Time_ns.now ()) t.last_cycle_finished_at)
                    max_inter_cycle_timeout
               then in_emacs_have_lock_do_cycle ()));
    (t.scheduler).have_lock_do_cycle <- Some request_emacs_run_cycle;
    (* The default [max_inter_cycle_timeout] is much smaller.  Setting it to 1s reduces
       load on emacs. *)
    Scheduler.set_max_inter_cycle_timeout
      (max_inter_cycle_timeout |> Time_ns.Span.to_span);
    (* Async would normally deal with errors that reach the main monitor by printing to
       stderr and then exiting 1.  This would look like an emacs crash to the user, so we
       instead output the error to the minibuffer. *)
    Async_kernel.Monitor.detach_and_iter_errors Async_kernel.Monitor.main ~f:(fun exn ->
      if have_active_env ()
      then Echo_area.message_s [%sexp (exn : exn)]
      else
        t.exceptions_raised_outside_emacs_env
        <- exn :: t.exceptions_raised_outside_emacs_env)
;;

module Expect_test_config = struct
  include Async.Expect_test_config

  (* In tests, we run the scheduler in the same thread as emacs. This means we don't have
     to signal emacs to run a cycle. Instead, we can directly run async cycles. *)

  let have_lock_do_cycle () =
    let thread_id = Thread.(id (self ())) in
    if t.emacs_thread_id = thread_id
    then
      (* In tests, the scheduler runs in the same thread as emacs. *)
      in_emacs_have_lock_do_cycle ()
  ;;

  let run f =
    (t.scheduler).have_lock_do_cycle <- Some have_lock_do_cycle;
    let deferred = f () in
    let rec run_cycles () =
      match Async.Deferred.peek deferred with
      | Some result -> result
      | None ->
        Scheduler.one_iter t.scheduler;
        run_cycles ()
    in
    run_cycles ()
  ;;
end

let shutdown () =
  let status = !Scheduler_status.status in
  Scheduler_status.status := Stopped;
  Cycle_requester.shutdown t.cycle_requester;
  (match t.keepalive_timer with
   | None -> ()
   | Some timer ->
     Timer.cancel timer;
     t.keepalive_timer <- None);
  match status with
  | Uninitialized | Stopped -> ()
  | Running ->
    (t.scheduler).have_lock_do_cycle
    <- Some
         (fun () ->
            Scheduler.unlock t.scheduler;
            raise_s [%sexp "Async shutdown"])
;;

let initialize () =
  lock_async_during_module_initialization ();
  Defun.defun_nullary_nil
    [%here]
    ~interactive:""
    ("ecaml-async-shutdown" |> Symbol.intern)
    shutdown;
  Defun.defun_nullary_nil
    [%here]
    ("ecaml-async-generate-cycle-report" |> Symbol.intern)
    ~interactive:""
    (fun () ->
       start_scheduler ();
       Async.don't_wait_for (Cycle_report.generate_report ()));
  let defun_benchmark ~name ~f =
    Defun.defun_nullary_nil [%here] (name |> Symbol.intern) ~interactive:"" (fun () ->
      let open Async in
      start_scheduler ();
      don't_wait_for
        (let%map time = f () in
         Echo_area.message_s time))
  in
  defun_benchmark
    ~name:"ecaml-async-benchmark-small-pings"
    ~f:Ecaml_bench.Bench_async_ecaml.benchmark_small_pings;
  defun_benchmark
    ~name:"ecaml-async-benchmark-throughput"
    ~f:Ecaml_bench.Bench_async_ecaml.benchmark_throughput
;;
