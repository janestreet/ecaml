open! Core
open! Async
open! Nested_profile
open! Nested_profile.Profile
open! Nested_profile.Profile.Private
open! Expect_test_helpers_core
open! Expect_test_helpers_async

let _ = Profile.should_profile := true
let sec = Time_ns.Span.of_sec

module Sync_or_async = Profile.Sync_or_async

module type Monad = sig
  include Monad.S

  val sync_or_async : _ t Sync_or_async.t
end

let clock = Private.Clock.create ~now:Time_ns.epoch
let advance_clock_by by = Clock.advance clock ~by
let () = Private.clock := clock

module Make_test (M : Monad) = struct
  open M.Let_syntax

  let test ?hide_if_less_than () =
    let non_async_profile () =
      profile
        ?hide_if_less_than
        Sync
        (lazy [%message "non_async_profile"])
        (fun () -> advance_clock_by (sec 0.1))
    in
    let bar () =
      profile
        M.sync_or_async
        ?hide_if_less_than
        (lazy [%message "bar"])
        (fun () ->
           advance_clock_by (sec 0.2);
           return ())
    in
    let baz () =
      profile
        M.sync_or_async
        ?hide_if_less_than
        (lazy [%message "baz"])
        (fun () ->
           let%bind () = bar () in
           non_async_profile ();
           advance_clock_by (sec 0.2);
           return ())
    in
    let foo () =
      profile
        M.sync_or_async
        ?hide_if_less_than
        (lazy [%message "foo"])
        (fun () ->
           let%bind () = bar () in
           let%bind () = baz () in
           let%bind () = bar () in
           advance_clock_by (sec 0.1);
           return ())
    in
    let%bind () = foo () in
    return ()
  ;;
end

module Test_profile = Make_test (struct
    include Monad.Ident

    let sync_or_async = Sync_or_async.Sync
  end)

module Test_profile_async = Make_test (struct
    include Deferred

    let sync_or_async = Sync_or_async.Async
  end)

let%expect_test "Non-async profile" =
  Test_profile.test ();
  [%expect
    {|
    (1_000_000us foo "1970-01-01 00:00:00Z" (
       ( 20%   200_000us bar)
       ( 50%   500_000us baz (
          ( 40%   200_000us bar)
          ( 20%   100_000us non_async_profile)
          ( 40%   200_000us gap)))
       ( 20%   200_000us bar)
       ( 10%   100_000us gap))) |}]
;;

let%expect_test "profile_async" =
  let%bind () = Test_profile_async.test () in
  [%expect
    {|
    (1_000_000us foo "1970-01-01 00:00:01Z" (
       ( 20%   200_000us bar)
       ( 50%   500_000us baz (
          ( 40%   200_000us bar)
          ( 20%   100_000us non_async_profile)
          ( 40%   200_000us gap)))
       ( 20%   200_000us bar)
       ( 10%   100_000us gap))) |}]
;;

let%expect_test "bad call to profile_async" =
  let child_exits = Ivar.create () in
  let%bind () =
    profile
      Async
      (lazy [%message "outer"])
      (fun () ->
         don't_wait_for
           (profile
              Async
              (lazy [%message "inner"])
              (fun () ->
                 advance_clock_by (sec 0.1);
                 Ivar.read child_exits));
         advance_clock_by (sec 0.01);
         return ())
  in
  let%bind () =
    [%expect
      {|
    ("Nested [profile Async] exited out-of-order." (message outer)
     (pending_children 1))
    (110_000us [1 pending child] outer "1970-01-01 00:00:02Z") |}]
  in
  (* The child exiting at this point will not lead to any output. *)
  Ivar.fill child_exits ();
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  let%bind () = [%expect {| |}] in
  (* A bad call doesn't corrupt the profile stack. *)
  let%bind () = Test_profile_async.test () in
  [%expect
    {|
    (1_000_000us foo "1970-01-01 00:00:02.11Z" (
       ( 20%   200_000us bar)
       ( 50%   500_000us baz (
          ( 40%   200_000us bar)
          ( 20%   100_000us non_async_profile)
          ( 40%   200_000us gap)))
       ( 20%   200_000us bar)
       ( 10%   100_000us gap))) |}]
;;

let%expect_test "parallel calls to profile_async" =
  let%bind () =
    profile
      Async
      (lazy [%message "outer"])
      (fun () ->
         advance_clock_by (sec 0.1);
         let%map () =
           profile
             Async
             (lazy [%message "inner-a"])
             (fun () ->
                advance_clock_by (sec 0.01);
                return ())
         and () =
           profile
             Async
             (lazy [%message "inner-b"])
             (fun () ->
                advance_clock_by (sec 0.01);
                return ())
         in
         advance_clock_by (sec 0.1))
  in
  [%expect
    {|
    (220_000us [parallel] outer "1970-01-01 00:00:03.11Z" (
       ( 45% 100_000us gap)
       (  9%  20_000us inner-a)
       (  9%  20_000us inner-b)
       ( 45% 100_000us gap))) |}]
;;


let%expect_test "inner ends after outer ends" =
  let%bind () =
    profile
      Async
      (lazy [%message "outer"])
      (fun () ->
         don't_wait_for
           (profile
              Async
              (lazy [%message "inner"])
              (fun () ->
                 Clock.advance clock ~by:(Time_ns.Span.of_ms 100.);
                 return ()));
         return ())
  in
  [%expect
    {|
    ("Nested [profile Async] exited out-of-order." (message outer)
     (pending_children 1))
    (100_000us [1 pending child] outer "1970-01-01 00:00:03.33Z") |}]
;;

let%expect_test "hide_if_less_than" =
  Test_profile.test () ~hide_if_less_than:(Time_ns.Span.of_ms 300.);
  [%expect
    {|
    (1_000_000us foo "1970-01-01 00:00:03.43Z" (
       ( 20%   200_000us gap)
       ( 50%   500_000us baz)
       ( 30%   300_000us gap))) |}]
;;

let%expect_test "hide_if_less_than async" =
  let%bind () =
    Test_profile_async.test () ~hide_if_less_than:(Time_ns.Span.of_ms 300.)
  in
  [%expect
    {|
    (1_000_000us foo "1970-01-01 00:00:04.43Z" (
       ( 20%   200_000us gap)
       ( 50%   500_000us baz)
       ( 30%   300_000us gap))) |}]
;;

let call_profile () =
  profile
    Sync
    (lazy [%sexp "context"])
    (fun () ->
       advance_clock_by (sec 1.);
       print_endline "function supplied to [profile] ran")
;;

let%expect_test "calling [profile] within [output_profile]" =
  Ref.set_temporarily
    output_profile
    (fun string ->
       call_profile ();
       print_endline string)
    ~f:(fun () ->
      Ref.set_temporarily hide_if_less_than Time_ns.Span.zero ~f:call_profile);
  [%expect
    {|
    function supplied to [profile] ran
    function supplied to [profile] ran
    (1_000_000us context "1970-01-01 00:00:05.43Z") |}]
;;

let%expect_test "calling [profile] within the [Sexp.t Lazy.t] supplied to [profile]" =
  profile
    Sync
    (lazy
      (call_profile ();
       [%sexp "outer context"]))
    (fun () ->
       advance_clock_by (sec 1.);
       print_endline "outer call");
  [%expect
    {|
    outer call
    function supplied to [profile] ran
    ((rendering_took 1_000_000us)
     (1_000_000us "outer context" "1970-01-01 00:00:07.43Z")) |}]
;;

let%expect_test "long line" =
  Ref.set_temporarily approximate_line_length_limit 10 ~f:(fun () ->
    profile
      Sync
      (lazy [%sexp "more than ten characters"])
      (fun () -> advance_clock_by (sec 1.)));
  [%expect {|
    (1_000_000us ... "1970-01-01 00:00:09.43Z") |}]
;;

let%expect_test "zero time" =
  Ref.set_temporarily hide_if_less_than (sec 0.) ~f:(fun () ->
    Ref.set_temporarily hide_top_level_if_less_than (sec 0.) ~f:(fun () ->
      profile
        Sync
        (lazy [%sexp "outer"])
        (fun () -> profile Sync (lazy [%sexp "inner"]) (fun () -> ()))));
  [%expect
    {|
    ((rendering_took 0us)
     (0us outer "1970-01-01 00:00:10.43Z" (
       (  _% 0us inner)))) |}]
;;

let%expect_test "[start_location := Line_preceding_profile]" =
  Ref.set_temporarily start_location Line_preceding_profile ~f:(fun () ->
    profile Sync (lazy [%sexp "foo"]) (fun () -> advance_clock_by (sec 1.)));
  [%expect {|
    ("1970-01-01 00:00:10.43Z"
     (1_000_000us foo)) |}]
;;

let%expect_test "we do not force the message in a profile that isn't rendered" =
  profile
    ~hide_if_less_than:(sec 0.)
    Sync
    (lazy
      (print_s [%message "forced"];
       [%sexp "foo"]))
    Fn.ignore;
  [%expect {| |}]
;;

let profile_sync () =
  profile Sync (lazy [%message "foo"]) (fun () -> advance_clock_by (sec 1.))
;;

let%expect_test "[sexp_of_time_ns] that raises" =
  require_does_not_raise [%here] ~cr:CR_soon (fun () ->
    Ref.set_temporarily
      sexp_of_time_ns
      (fun _ -> raise_s [%message "raising"])
      ~f:profile_sync);
  [%expect
    {|
    (1_000_000us foo ("[Profile.sexp_of_time_ns] raised"(exn raising)(backtrace("<backtrace elided in test>")))) |}]
;;

let%expect_test "[output_profile] that raises" =
  require_does_not_raise [%here] ~cr:CR_soon (fun () ->
    Ref.set_temporarily
      output_profile
      (fun _ -> raise_s [%message "raising"])
      ~f:profile_sync);
  [%expect
    {|
    ("[Profile.output_profile] raised" (exn raising)
     (backtrace ("<backtrace elided in test>"))) |}]
;;

let%expect_test "[profile] message that raises" =
  require_does_not_raise [%here] ~cr:CR_soon (fun () ->
    profile
      Sync
      (lazy (raise_s [%message "raising"]))
      (fun () -> advance_clock_by (sec 1.)));
  [%expect
    {|
    (1_000_000us ("[Profile.profile] message raised" (exn raising) (backtrace ("<backtrace elided in test>"))) "1970-01-01 00:00:13.43Z") |}]
;;

let%expect_test "[backtrace]" =
  let test_backtrace ~should_profile =
    Ref.set_temporarily Profile.should_profile should_profile ~f:(fun () ->
      profile
        Sync
        (lazy [%message "Outer profile"])
        (fun () ->
           profile
             Sync
             (lazy [%message "Inner profile"])
             (fun () ->
                print_s
                  [%sexp
                    "Backtrace from inner function", (backtrace () : Sexp.t list option)]);
           print_s
             [%sexp
               "Backtrace from outer function", (backtrace () : Sexp.t list option)]))
  in
  test_backtrace ~should_profile:true;
  let%bind () =
    [%expect
      {|
    ("Backtrace from inner function" (("Inner profile" "Outer profile")))
    ("Backtrace from outer function" (("Outer profile"))) |}]
  in
  test_backtrace ~should_profile:false;
  [%expect
    {|
    ("Backtrace from inner function" ())
    ("Backtrace from outer function" ()) |}]
;;

let%expect_test "[tag_frames_with]" =
  Ref.set_temporarily tag_frames_with (Some (fun _ -> None)) ~f:call_profile;
  let%bind () =
    [%expect
      {|
    function supplied to [profile] ran
    (1_000_000us context "1970-01-01 00:00:14.43Z") |}]
  in
  Ref.set_temporarily
    tag_frames_with
    (Some (fun _ -> Some (Atom "hello world")))
    ~f:call_profile;
  [%expect
    {|
    function supplied to [profile] ran
    (1_000_000us (context "hello world") "1970-01-01 00:00:15.43Z") |}]
;;

let%expect_test "[tag_frames_with] runs before f" =
  let some_int = ref 0 in
  Ref.set_temporarily
    tag_frames_with
    (Some (fun _ -> Some [%sexp { some_int : int = !some_int }]))
    ~f:(fun () ->
      profile
        Sync
        (lazy [%message "Increment some_int"])
        (fun () ->
           incr some_int;
           advance_clock_by (sec 1.)));
  [%expect
    {|
      (1_000_000us ("Increment some_int" ((some_int 0))) "1970-01-01 00:00:16.43Z") |}]
;;

let%expect_test "[tag_frames_with] raises" =
  Ref.set_temporarily
    tag_frames_with
    (Some (fun _ -> raise_s [%sexp "Hello world"]))
    ~f:call_profile;
  [%expect
    {|
    function supplied to [profile] ran
    (1_000_000us (context ("[Profile.tag_frames_with] raised" (exn "Hello world") (backtrace ("<backtrace elided in test>")))) "1970-01-01 00:00:17.43Z") |}]
;;

let%expect_test "[am_forcing_message]" =
  let sexp () = [%sexp (am_forcing_message () : bool)] in
  print_s (sexp ());
  let%bind () = [%expect {| false |}] in
  profile Sync (lazy (sexp ())) (fun () -> advance_clock_by (sec 1.));
  let%bind () = [%expect {| (1_000_000us true "1970-01-01 00:00:18.43Z") |}] in
  return ()
;;
