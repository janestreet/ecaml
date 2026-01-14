open! Core
open! Async_kernel
open! Import
open! Nested_profile
open! Ecaml_profile

let sec = Time_ns.Span.of_sec

module Clock = Profile.Private.Clock

let show_profile_and_flush () =
  Current_buffer.set_temporarily Sync (Buffer.find_exn ~name:"*profile*") ~f:(fun () ->
    let contents = Current_buffer.contents () in
    Current_buffer.inhibit_read_only Sync (fun () -> Current_buffer.erase ());
    contents)
  |> Text.to_utf8_bytes
  |> print_endline
;;

let test_setup_minimal ~f =
  let clock = Clock.create ~now:Time_ns.epoch in
  Ref.sets_temporarily [ T (Profile.Private.clock, clock) ] ~f:(fun () -> f clock);
  show_profile_and_flush ()
;;

let test_setup ~f =
  test_setup_minimal ~f:(fun clock ->
    Ref.sets_temporarily
      [ T (Profile.hide_top_level_if_less_than, Time_ns.Span.zero)
      ; T (Profile.hide_if_less_than, Time_ns.Span.zero)
      ]
      ~f:(fun () -> f clock))
;;

let%expect_test _ =
  test_setup ~f:(fun clock ->
    profile Sync [%lazy_sexp "nest"] (fun () ->
      Clock.advance clock ~by:(sec 0.1);
      Elisp_gc.garbage_collect ();
      Clock.advance clock ~by:(sec 0.2);
      Elisp_gc.garbage_collect ();
      Clock.advance clock ~by:(sec 0.3)));
  [%expect
    {|
    ((rendering_took 0us)
     (620_000us nest (19:00:00.000000000-05:00 1969-12-31) (
       ( 16% 100_000us gap)
       (  2%  10_000us (garbage-collect) (
          (  0%       0us (boundp gc-elapsed))
          (  0%       0us (symbol-value gc-elapsed))
          (100%  10_000us (gc (gcs_done _)))))
       ( 32% 200_000us gap)
       (  2%  10_000us (garbage-collect) (
          (  0%       0us (boundp gc-elapsed))
          (  0%       0us (symbol-value gc-elapsed))
          (100%  10_000us (gc (gcs_done _)))))
       ( 48% 300_000us gap))))
    |}];
  return ()
;;

let%expect_test "tag-frame-function" =
  test_setup_minimal ~f:(fun clock ->
    Current_buffer.set_value_temporarily
      Sync
      Ecaml_profile.Private.tag_function
      (Some
         (Defun.lambda_nullary [%here] (Returns Value.Type.string) (fun () ->
            "hello world")))
      ~f:(fun () ->
        profile Sync [%lazy_sexp "nest"] (fun () -> Clock.advance clock ~by:(sec 0.3))));
  [%expect {| (300_000us (nest "hello world") (19:00:00.000000000-05:00 1969-12-31)) |}];
  return ()
;;

let show_in_message value =
  test_setup ~f:(fun _ -> profile Sync [%lazy_sexp (value : Value.t)] ignore)
;;

let%expect_test "[Value.sexp_of_t] respect [print_length] in profile records" =
  let test size =
    show_in_message (List.init size ~f:Fn.id |> Value.Type.(to_value (list int)))
  in
  test 10;
  [%expect
    {|
    ((rendering_took 0us)
     (0us (0 1 2 3 4 5 6 7 8 9 . ...) (19:00:00.000000000-05:00 1969-12-31)))
    |}];
  Customization.set_value_temporarily print_length (Some 3) ~f:(fun () -> test 10);
  [%expect
    {|
    ((rendering_took 0us)
     (0us (0 1 2 . ...) (19:00:00.000000000-05:00 1969-12-31)))
    |}];
  return ()
;;

let%expect_test "[Value.sexp_of_t] respect [print_level] in profile records" =
  let rec value i =
    if i = 0
    then Value.nil
    else (
      let v = value (i - 1) in
      Value.cons v v)
  in
  let v7 = value 7 in
  show_in_message v7;
  [%expect
    {|
    ((rendering_took 0us)
     (0us (((((((nil) nil) (nil) nil) ((nil) nil) (nil) nil) (((nil) nil) (nil) nil) ((nil) nil) (nil) nil) ((((nil) nil) (nil) nil) ((nil) nil) (nil) nil) (((nil) nil) (nil) nil) ((nil) nil) (nil) nil) (((((nil) nil) (nil) nil) ((nil) nil) (nil) nil) (((nil) nil) (nil) nil) ((nil) nil) (nil) nil) ((((nil) nil) (nil) nil) ((nil) nil) (nil) nil) (((nil) nil) (nil) nil) ((nil) nil) (nil) nil) (19:00:00.000000000-05:00 1969-12-31)))
    |}];
  Customization.set_value_temporarily print_level (Some 3) ~f:(fun () ->
    show_in_message v7);
  [%expect
    {|
    ((rendering_took 0us)
     (0us (((... ... ... ... ...) (... ... ... ...) (... ... ...) (... ...) (...) nil) ((... ... ... ...) (... ... ...) (... ...) (...) nil) ((... ... ...) (... ...) (...) nil) ((... ...) (...) nil) ((...) nil) (nil) nil) (19:00:00.000000000-05:00 1969-12-31)))
    |}];
  return ()
;;

(* Previously, this didn't correctly raise when profiling was enabled, because the error
   was cleared when the profiling code itself calls back into Emacs (to serialize funcall
   arguments to sexps).

   This affected any Value.funcall*s where (1) profiling is enabled and would trigger on
   that funcall, based on the elapsed time; (2) the funcall exits nonlocally
   (signal/throw); and (3) the Value.funcall's profile entry is toplevel.

   The fact that (3) is uncommon is probably the only reason we hadn't noticed this bug
   earlier, but (3) can happen when a user calls an Ecaml-implemented command whose
   [interactive] form directly contains a [Value.funcall]. Since the Ecaml-implemented
   command hasn't started yet (call-interactively is still computing its arguments), the
   [interactive] form is toplevel, and if it (a) signals and (b) takes enough time to
   trigger profiling, Profile.Record.to_string_hum will swallow the error (which gets
   incorrectly surfaced as "[Profile.profile] message raised").

   An easy way to satisfy conditions (a) and (b) is to have the interactive form prompt
   the user, and then for the user to press C-g. *)
let%expect_test "test Value.funcall that raises, with and without profiling" =
  let error = Value.intern "error" in
  let arg = Value.of_utf8_bytes "dummy error message" in
  test_setup ~f:(fun _ ->
    Ref.sets_temporarily
      [ T (Nested_profile.Profile.sexp_of_time_ns, sexp_of_opaque)
      ; T (Nested_profile.Profile.never_show_rendering_took, true)
      ; T
          ( Nested_profile.Profile.output_profile
          , fun string ->
              if String.is_substring string ~substring:"[Profile.profile]"
              then
                print_string
                  (Regexp.replace_string
                     (Regexp.of_rx
                        (Seq
                           [ One_or_more (Any_in [ Named Digit; Chars_in "_" ])
                           ; Exactly "us"
                           ]))
                     ~with_:"<time span>"
                     ~in_:string) )
      ]
      ~f:(fun () ->
        require_does_raise (fun () -> Value.funcall1 error arg ~should_profile:false);
        [%expect {| ("dummy error message") |}];
        require_does_raise (fun () -> Value.funcall1 error arg ~should_profile:true);
        [%expect {| ("dummy error message") |}]));
  return ()
;;
