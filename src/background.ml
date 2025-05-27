open! Core
open! Async
open! Import0

let background_job_key =
  Univ_map.Key.create ~name:"Background Async job" [%sexp_of: Source_code_position.t]
;;

let mark_running_in_background ~(here : [%call_pos]) f =
  Nested_profile.Profile.disown (fun () ->
    Scheduler.with_local background_job_key (Some here) ~f)
;;

let mark_running_in_foreground f = Scheduler.with_local background_job_key None ~f
let currently_running_in_background () = Scheduler.find_local background_job_key
let am_running_in_background () = is_some (currently_running_in_background ())
let am_running_in_foreground () = is_none (currently_running_in_background ())

let schedule_foreground_block_on_async
  ~(here : [%call_pos])
  ?raise_exceptions_to_monitor
  f
  =
  if am_running_in_foreground ()
  then
    raise_s
      [%sexp
        "Assertion failed -- [Background.schedule_foreground_block_on_async] called from \
         foreground job"
        , (here : Source_code_position.t)];
  Value.Private.enqueue_foreground_block_on_async
    ~here
    ?raise_exceptions_to_monitor
    (fun () ->
       Nested_profile.Profile.disown (fun () ->
         Nested_profile.Profile.profile
           Async
           [%lazy_message
             "[Background.schedule_foreground_block_on_async]"
               (here : Source_code_position.t)]
           (fun () -> mark_running_in_foreground f)))
;;

let don't_wait_for ~(here : [%call_pos]) f =
  mark_running_in_background ~here (fun () ->
    let monitor = Monitor.create ~here:[%here] ~name:"background_monitor" () in
    Monitor.detach_and_iter_errors monitor ~f:(fun exn ->
      message_s
        [%message
          "background job raised"
            ~job_created_at:(here : Source_code_position.t opaque_in_test)
            ~_:(Monitor.extract_exn exn : exn)]);
    Scheduler.within ~monitor (fun () -> don't_wait_for (f ())))
;;

module Clock = struct
  let every'
    ?start
    ?stop
    ?continue_on_error
    ?finished
    ?(should_profile = true)
    ~(here : [%call_pos])
    interval
    f
    =
    mark_running_in_background ~here (fun () ->
      Clock_ns.every' ?start ?stop ?continue_on_error ?finished interval (fun () ->
        match should_profile with
        | false -> f ()
        | true ->
          Nested_profile.Profile.profile
            Async
            [%lazy_message "[Background.Clock.every']" (here : Source_code_position.t)]
            f))
  ;;

  let every
    ?start
    ?stop
    ?continue_on_error
    ?(should_profile = true)
    ~(here : [%call_pos])
    interval
    f
    =
    mark_running_in_background ~here (fun () ->
      Clock_ns.every ?start ?stop ?continue_on_error interval (fun () ->
        match should_profile with
        | false -> f ()
        | true ->
          Nested_profile.Profile.profile
            Sync
            [%lazy_message "[Background.clock.every]" (here : Source_code_position.t)]
            f))
  ;;
end

let assert_foreground ?message ~here:(assertion_failed_at : [%call_pos]) () =
  match currently_running_in_background () with
  | None -> ()
  | Some background_job_started_at ->
    raise_s
      [%sexp
        ("Assertion failed -- running in background job" : string)
        , { background_job_started_at : Source_code_position.t
          ; assertion_failed_at : Source_code_position.t
          }
        , (message : (Sexp.t option[@sexp.option]))]
;;

module Private = struct
  let mark_running_in_background = mark_running_in_background
  let mark_running_in_foreground = mark_running_in_foreground
  let schedule_foreground_block_on_async = schedule_foreground_block_on_async
end

let schedule_foreground_block_on_async =
  schedule_foreground_block_on_async ?raise_exceptions_to_monitor:None
;;
