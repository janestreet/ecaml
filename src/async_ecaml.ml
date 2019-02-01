(* This module adds support for running Async code in Ecaml.

   We change the way the scheduler works.  The Async scheduler runs in its own thread, but
   all Async cycles now run within the main emacs thread.  When the scheduler would run a
   cycle, it instead sends a packet to a socket that the emacs main thread listens to
   (which notifies emacs that it should run a cycle), and waits for the cycle to be run.
   This way, whenever we run Ecaml code, we have both the Emacs [active_env] and the Async
   lock.  This way, it is always safe to call Ecaml functions from Async, and to modify
   Async data structures from Ecaml. *)

module Ecaml_filename = Filename
open! Core_kernel
open! Import
module Ivar = Async.Ivar
module Mutex = Core.Mutex
module Time = Core.Time
module Unix = Core.Unix
module Scheduler = Async_unix.Async_unix_private.Raw_scheduler

let message_s = Echo_area.message_s

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
    let%map () = Clock_ns.after (sec_ns 10.) in
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

(** [Thread_safe_sleeper] is a thread-safe data structure for use by threads holding
    the Async lock.  The usage pattern is that one thread calls [blocking_sleep], which
    causes it to release the Async lock (and OCaml lock) and block.  Later, another
    thread calls [wake_up], after which the blocked thread wakes up and reacquires
    the locks. *)
module Thread_safe_sleeper : sig
  type t [@@deriving sexp_of]

  val create : unit -> t

  (** [blocking_sleep] assumes [Scheduler.am_holding_lock scheduler]. It unlocks the
      scheduler before sleeping, and re-locks the scheduler as soon as it wakes up. *)
  val blocking_sleep : t -> unit

  val wake_up : t -> unit
end = struct
  type t =
    { mutex : Mutex.t sexp_opaque
    ; wake_up : Condition.t sexp_opaque
    ; scheduler : Scheduler.t
    }
  [@@deriving sexp_of]

  let create () =
    { mutex = Mutex.create (); wake_up = Condition.create (); scheduler = Scheduler.t () }
  ;;

  let critical_section t ~f =
    assert (Scheduler.am_holding_lock t.scheduler);
    Mutex.critical_section t.mutex ~f
  ;;

  let blocking_sleep t =
    (* We unlock the Async lock while in a critical section rather than before
       the critical section to avoid a race in which:
       - Thread 1 requests service from Thread 2
       - Thread 1 calls [blocking_sleep]
       - Thread 1 unlocks the Async lock
       - Thread 2 locks the Async lock
       - Thread 2 performs service and calls [wake_up]
       - Thread 2 unlocks the Async lock
       - Thread 1 calls [wait], and is then stuck. *)
    critical_section t ~f:(fun () ->
      Scheduler.unlock t.scheduler;
      Condition.wait t.wake_up t.mutex);
    Scheduler.lock t.scheduler
  ;;

  let wake_up t = critical_section t ~f:(fun () -> Condition.broadcast t.wake_up)
end

(** The Cycle_requester is a mechanism for requesting emacs to run a cycle by writing a
    byte along a socket. Emacs passes this byte on to the process filter we define, which
    runs a cycle in the emacs thread.

    We maintain the invariant that there is at most one byte waiting to be read at a
    time. *)
module Cycle_requester : sig
  type t

  val byte_was_probably_lost : t -> unit
  val create : unit -> t
  val request_cycle : t -> unit
  val register_cycle_handler : t -> (unit -> unit) -> unit
  val shutdown : t -> unit
end = struct
  type t =
    { mutable client_process : Process.t option
    ; mutable exists_unread_byte : bool
    ; mutable server_process : Process.t option
    ; write_to_request_cycle : Unix.File_descr.t
    }

  let byte_was_probably_lost t = t.exists_unread_byte <- false

  let create () =
    let write_to_request_cycle =
      Unix.socket ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0
    in
    { client_process = None
    ; exists_unread_byte = false
    ; server_process = None
    ; write_to_request_cycle
    }
  ;;

  let request_cycle t =
    assert (Scheduler.am_holding_lock (Scheduler.t ()));
    (* Ensure we write at most 1 byte to the socket between runs of the cycle handler.
       [request_cycle] might be called multiple times.  In extreme cases, writing more
       than one byte to the socket could lead to the socket's buffer filling up and
       [write] blocking, deadlocking emacs. *)
    if not t.exists_unread_byte
    then (
      try
        ignore
          (Unix.single_write t.write_to_request_cycle ~buf:(Bytes.of_string "\x05") : int);
        t.exists_unread_byte <- true
      with
      | _ ->
        (* We ignore exceptions here, because the network socket may be closed, e.g. when
           Emacs is shutting down, and we don't want to fail in that case. *)
        ())
  ;;

  let with_current_dir dir ~f =
    let saved_dir = Unix.getcwd () in
    Unix.chdir dir;
    Exn.protect ~f ~finally:(fun () -> Unix.chdir saved_dir)
  ;;

  let register_cycle_handler t run_cycle =
    let tmpdir =
      Ecaml_filename.to_directory
        (Option.value (System.getenv ~var:"TMPDIR") ~default:"/tmp")
    in
    (* If [String.length tmpdir > 108], then creating a unix socket at that path fails
       with "Service name too long".  To avoid this, we chdir and create the socket using
       a relative path. *)
    with_current_dir tmpdir ~f:(fun () ->
      let socket_path = ".ecaml." ^ (Unix.getpid () |> Pid.to_string) in
      let server_process =
        Process.create_unix_network_process
          ()
          ~name:"Async scheduler"
          ~socket_path
          ~filter:(fun client_process _ ->
            assert (Scheduler.am_holding_lock (Scheduler.t ()));
            t.exists_unread_byte <- false;
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
      t.server_process <- Some server_process)
  ;;

  let shutdown t =
    Option.iter t.client_process ~f:Process.kill;
    Option.iter t.server_process ~f:Process.kill
  ;;
end

(* A [Pending_emacs_call.t] is a function that should be run outside of any Async job and
   without the Async lock, but reports its result back into Async.  To ensure that pending
   emacs calls are run in a timely manner, we run them whenever we run Async cycles, and
   we request Async cycles whenever we enqueue a pending emacs call. *)
module Pending_emacs_call = struct
  type 'a call =
    { f : unit -> 'a
    ; result : ('a, exn) Result.t Ivar.t
    }

  type t = T : 'a call -> t
end

type t =
  { (* [am_running_async_cycle] is set to [true] while we're running an Async cycle.
       During an Async cycle, we avoid running nested Async cycles or
       [block_on_async]s. *)
    mutable am_running_async_cycle : bool
  ; cycle_done_sleeper : Thread_safe_sleeper.t
  ; cycle_requester : Cycle_requester.t
  ; emacs_thread_id : int
  ; mutable exceptions_raised_outside_emacs_env : exn list
  ; mutable keepalive_timer : Timer.t option
  ; mutable last_cycle_finished_at : Time_ns.t
  ; scheduler : Scheduler.t
  ; mutable pending_emacs_calls : Pending_emacs_call.t Queue.t
  }

let t =
  { am_running_async_cycle = false
  ; cycle_done_sleeper = Thread_safe_sleeper.create ()
  ; cycle_requester = Cycle_requester.create ()
  ; emacs_thread_id = Thread.(id (self ()))
  ; exceptions_raised_outside_emacs_env = []
  ; keepalive_timer = None
  ; last_cycle_finished_at = Time_ns.epoch
  ; scheduler = Scheduler.t ()
  ; pending_emacs_calls = Queue.create ()
  }
;;

let run_pending_emacs_calls () =
  let has_work_to_do = not (Queue.is_empty t.pending_emacs_calls) in
  if has_work_to_do
  then (
    let pending_calls = t.pending_emacs_calls in
    t.pending_emacs_calls <- Queue.create ();
    Queue.iter pending_calls ~f:(fun (Pending_emacs_call.T pending_emacs_call) ->
      Scheduler.unlock t.scheduler;
      let result = Result.try_with pending_emacs_call.f in
      Scheduler.lock t.scheduler;
      Ivar.fill pending_emacs_call.result result));
  has_work_to_do
;;

(* When the scheduler requests an Async cycle, [in_emacs_have_lock_do_cycle] runs the
   cycle inside the emacs thread and notifies the scheduler when it is finished. *)
let in_emacs_have_lock_do_cycle () =
  if not (Value.Expert.have_active_env ()) || not (Scheduler.am_holding_lock t.scheduler)
  then raise_s [%sexp "[in_emacs_have_lock_do_cycle] should only be called by emacs"];
  (* If we are already running an Async cycle, then we can't start a new one, so we do
     nothing.  We can reach here with [t.am_running_async_cycle = true] if Ecaml calls a
     blocking Elisp function without using [run_outside_async].  We are in the middle of a
     long, possibly unending, code transition in which we are wrapping such calls with
     [run_outside_async]. *)
  if not t.am_running_async_cycle
  then (
    if debug
    then Debug.eprint_s [%message "running a cycle" ~time:(Time.now () : Time.t)];
    List.iter t.exceptions_raised_outside_emacs_env ~f:(fun exn ->
      Echo_area.message_s [%sexp (exn : exn)]);
    t.exceptions_raised_outside_emacs_env <- [];
    let time = Time.now () in
    Exn.protect
      ~f:(fun () ->
        Async.Unix.Private.Wait.check_all ();
        let rec run_cycles max_cycles =
          (* Pending emacs calls may have been enqueued from outside of Async. Run them so
             their deferreds get filled. *)
          let ran_pending_calls = run_pending_emacs_calls () in
          t.am_running_async_cycle <- true;
          Exn.protect
            ~f:(fun () -> Async_kernel.Async_kernel_scheduler.(run_cycle (t ())))
            ~finally:(fun () -> t.am_running_async_cycle <- false);
          if max_cycles > 0 && (ran_pending_calls || Scheduler.num_pending_jobs () > 0)
          then run_cycles (max_cycles - 1)
        in
        (* 5 was chosen as an arbitrary limit to prevent the emacs toplevel from being
           starved if an Async job misbehaves. *)
        run_cycles 5)
      ~finally:(fun () ->
        t.last_cycle_finished_at <- Time_ns.now ();
        if debug
        then
          Debug.eprint_s
            [%message "cycle took" ~time:(Time.diff (Time.now ()) time : Time.Span.t)];
        Thread_safe_sleeper.wake_up t.cycle_done_sleeper))
;;

(* [request_emacs_run_cycle] requests the emacs main thread to run a cycle. It hands over
   the Async lock in the process. *)
let request_emacs_run_cycle scheduler_thread_id () =
  assert (Scheduler.am_holding_lock t.scheduler);
  Cycle_requester.request_cycle t.cycle_requester;
  (* Async helper threads call [request_emacs_run_cycle], and we don't want those to
     block; we want only the scheduler to block. *)
  if Thread.(id (self ())) = scheduler_thread_id
  then (
    let start = Time_ns.now () in
    Thread_safe_sleeper.blocking_sleep t.cycle_done_sleeper;
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
    assert (Scheduler.am_holding_lock t.scheduler);
    Scheduler_status.status := Running;
    Async.Unix.Private.Wait.do_not_handle_sigchld ();
    if debug
    then Debug.eprint_s [%message "initializing async" [%here] (Time.now () : Time.t)];
    (* We hold the Async lock, so it should be impossible for the scheduler to try to run
       a cycle. *)
    (t.scheduler).have_lock_do_cycle
    <- Some (fun () -> raise_s [%message "BUG in Async_ecaml" [%here]]);
    let scheduler_thread =
      Thread.create
        (fun () ->
           match Scheduler.go () ~raise_unhandled_exn:true with
           | _ -> .
           | exception exn ->
             (match !Scheduler_status.status with
              (* If we requested the scheduler to stop, this exception is expected. *)
              | Stopped -> ()
              | Running | Uninitialized -> raise exn))
        ()
    in
    (* We set [have_lock_do_cycle] as early as possible so that the Async scheduler runs
       cycles in the desired way, even if later parts of initialization raise. *)
    (t.scheduler).have_lock_do_cycle
    <- Some (request_emacs_run_cycle (Thread.id scheduler_thread));
    Defun.defun
      Q.ecaml_async_take_lock_do_cycle
      [%here]
      ~interactive:No_arg
      (Returns Value.Type.unit)
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
            [%here]
            max_inter_cycle_timeout
            ~repeat:max_inter_cycle_timeout
            ~name:("async-ecaml-keepalive-timer" |> Symbol.intern)
            ~f:(fun () ->
              try
                if Time_ns.Span.( >= )
                     (Time_ns.diff (Time_ns.now ()) t.last_cycle_finished_at)
                     max_inter_cycle_timeout
                then (
                  Cycle_requester.byte_was_probably_lost t.cycle_requester;
                  in_emacs_have_lock_do_cycle ())
              with
              | exn ->
                Echo_area.message_s [%sexp "Error in async keepalive timer", (exn : exn)]));
    Set_once.set_exn Ecaml_callback.set_async_execution_context [%here] (fun () ->
      if not t.am_running_async_cycle
      then
        Async_kernel.Async_kernel_scheduler.(
          set_execution_context (t ()) main_execution_context));
    (* The default [max_inter_cycle_timeout] is much smaller.  Setting it to 1s reduces
       load on emacs. *)
    Scheduler.set_max_inter_cycle_timeout
      (max_inter_cycle_timeout |> Time_ns.Span.to_span_float_round_nearest);
    (* [Async_unix] installs a handler for logging exceptions raised to try-with that has
       already returned.  That logs to stderr, which doesn't work well in Emacs.  So we
       install a handler that reports the error with [message_s]. *)
    (Async_kernel.Monitor.Expert.try_with_log_exn :=
       fun exn ->
         message_s
           [%message
             "Exception raised to [Monitor.try_with] that already returned."
               ~_:(exn : exn)]);
    (* Async would normally deal with errors that reach the main monitor by printing to
       stderr and then exiting 1.  This would look like an emacs crash to the user, so we
       instead output the error to the minibuffer. *)
    Async_kernel.Monitor.detach_and_iter_errors Async_kernel.Monitor.main ~f:(fun exn ->
      if Value.Expert.have_active_env ()
      then
        (* We really want to see the error, so we inhibit quit while displaying it. *)
        Current_buffer.set_value_temporarily Command.inhibit_quit true ~f:(fun () ->
          message_s [%sexp (exn : exn)])
      else
        t.exceptions_raised_outside_emacs_env
        <- exn :: t.exceptions_raised_outside_emacs_env)
;;

module Private = struct
  module Context_backtrace = struct
    type t = (Source_code_position.t * Sexp.t Lazy.t) list [@@deriving sexp_of]
  end

  let context_backtrace : Context_backtrace.t ref = ref []

  let block_on_async here ?context f =
    assert (Scheduler.am_holding_lock t.scheduler);
    Ref.set_temporarily
      context_backtrace
      ((here, Option.value context ~default:(lazy [%message])) :: !context_backtrace)
      ~f:(fun () ->
        if t.am_running_async_cycle
        then
          raise_s
            [%message.omit_nil
              "Called [block_on_async] in the middle of an Async job!"
                (context_backtrace : Context_backtrace.t ref)];
        let rec run_cycles_until_filled deferred =
          if Command.quit_requested ()
          then error_s [%message "Blocking operation interrupted"]
          else (
            match Async.Deferred.peek deferred with
            | Some result -> result
            | None ->
              (* [Thread.delay] gives the scheduler thread time to run before we run a
                 cycle. *)
              Scheduler.unlock t.scheduler;
              Thread.delay (Time.Span.of_us 10. |> Time.Span.to_sec);
              Scheduler.lock t.scheduler;
              in_emacs_have_lock_do_cycle ();
              run_cycles_until_filled deferred)
        in
        let deferred =
          Async.(
            Monitor.try_with ~extract_exn:true ~run:`Schedule f
            >>| Or_error.of_exn_result)
        in
        let result = run_cycles_until_filled deferred in
        match result with
        | Ok x -> x
        | Error error -> Error.raise error)
  ;;

  let () = Set_once.set_exn Defun.Private.block_on_async [%here] block_on_async

  let run_outside_async f =
    let open Async in
    Deferred.create (fun result ->
      Queue.enqueue t.pending_emacs_calls (T { f; result });
      (* We request an Async cycle to ensure the pending call is run in a timely
         manner. *)
      Cycle_requester.request_cycle t.cycle_requester)
    >>| Result.ok_exn
  ;;
end

module Expect_test_config = struct
  include Async.Expect_test_config

  let run f =
    Private.block_on_async [%here] ~context:(lazy [%message "Expect_test_config.run"]) f
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
  start_scheduler ();
  lock_async_during_module_initialization ();
  Defun.defun_nullary_nil
    ("ecaml-async-shutdown" |> Symbol.intern)
    [%here]
    ~interactive:No_arg
    shutdown;
  Defun.defun_nullary_nil
    ("ecaml-async-generate-cycle-report" |> Symbol.intern)
    [%here]
    ~interactive:No_arg
    (fun () -> Async.don't_wait_for (Cycle_report.generate_report ()));
  let defun_benchmark ~name ~f =
    Defun.defun_nullary
      (name |> Symbol.intern)
      [%here]
      ~interactive:No_arg
      Returns_unit_deferred
      (fun () ->
         let open Async in
         let%map time = f () in
         Echo_area.message_s time)
  in
  defun_benchmark
    ~name:"ecaml-async-benchmark-small-pings"
    ~f:Ecaml_bench.Bench_async_ecaml.benchmark_small_pings;
  defun_benchmark
    ~name:"ecaml-async-benchmark-throughput"
    ~f:Ecaml_bench.Bench_async_ecaml.benchmark_throughput;
  Defun.defun_nullary
    ("ecaml-async-test-block-forever" |> Symbol.intern)
    [%here]
    ~interactive:No_arg
    Returns_unit_deferred
    (fun () ->
       message_s [%message "blocking forever -- press C-g to interrupt"];
       Async.Deferred.never ());
  Defun.defun_nullary_nil
    ("ecaml-async-test-execution-context-handling" |> Symbol.intern)
    [%here]
    ~interactive:No_arg
    (fun () ->
       let open Async in
       let test_passed = ref true in
       let check_execution_context () =
         let execution_context = Scheduler.current_execution_context () in
         if not (phys_equal execution_context Execution_context.main)
         then (
           test_passed := false;
           Echo_area.message_s
             [%message
               "Ecaml callback not running in main execution context"
                 (execution_context : Execution_context.t)])
       in
       check_execution_context ();
       let timer =
         Timer.run_after
           ~repeat:(sec_ns 0.1)
           [%here]
           (sec_ns 0.1)
           ~f:check_execution_context
           ~name:("check-execution-context-timer" |> Symbol.intern)
       in
       don't_wait_for
         (let%map _ignored =
            Monitor.try_with (fun () ->
              let%bind () = Clock.after (sec 0.1) in
              let%bind () = Clock.after (sec 2.) in
              Timer.cancel timer;
              Echo_area.messagef
                "Execution-context test %s"
                (if !test_passed then "passed" else "failed");
              return ())
          in
          ()));
  Defun.defun_nullary_nil
    ("ecaml-async-test-in-thread-run" |> Symbol.intern)
    [%here]
    ~docstring:"Call [In_thread.run] a number of times and report on its performance."
    ~interactive:No_arg
    (fun () ->
       let open Async in
       don't_wait_for
         (let open Deferred.Let_syntax in
          message_s [%message "testing"];
          let all_elapsed = ref [] in
          let long_cutoff = sec_ns 0.01 in
          let rec loop i =
            if i = 0
            then (
              let all_elapsed =
                List.sort
                  ~compare:Time_ns.Span.compare
                  (let x = !all_elapsed in
                   all_elapsed := [];
                   x)
              in
              message_s [%message "test finished" (all_elapsed : Time_ns.Span.t list)];
              return ())
            else (
              let before = Time_ns.now () in
              let%bind () = In_thread.run (fun () -> Thread.yield ()) in
              let elapsed = Time_ns.diff (Time_ns.now ()) before in
              all_elapsed := elapsed :: !all_elapsed;
              if Time_ns.Span.( >= ) elapsed long_cutoff
              then message_s [%message "Slow [In_thread.run]" (elapsed : Time_ns.Span.t)];
              loop (i - 1))
          in
          loop 100))
;;
