open! Core_kernel
open! Async_kernel
open! Import
open! Nested_profile
open! Ecaml_profile

let sec = Time_ns.Span.of_sec

module Set = struct
  type t = T : ('a ref * 'a) -> t

  let save_current ts = List.map ts ~f:(fun (T (r, _)) -> T (r, !r))
  let sets ts = List.iter ts ~f:(fun (T (r, a)) -> r := a)

  let set_temporarily ts ~f =
    let saved = save_current ts in
    sets ts;
    protect ~f ~finally:(fun () -> sets saved)
  ;;
end

module Clock = Profile.Private.Clock

let test_setup ~f =
  let clock = Clock.create ~now:Time_ns.epoch in
  Set.set_temporarily
    [ T (Profile.should_profile, true)
    ; T (Profile.hide_top_level_if_less_than, Time_ns.Span.zero)
    ; T (Profile.hide_if_less_than, Time_ns.Span.zero)
    ; T (Profile.output_profile, fun string -> message string)
    ; T (Profile.Private.clock, clock)
    ]
    ~f:(fun () -> f clock)
;;

let%expect_test _ =
  test_setup ~f:(fun clock ->
    profile
      Sync
      (lazy [%sexp "nest"])
      (fun () ->
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
       ( 48% 300_000us gap)))) |}];
  return ()
;;

let%expect_test "tag-frame-function" =
  Current_buffer.set_value_temporarily
    Sync
    Ecaml_profile.Private.tag_function
    (Some
       (Defun.lambda_nullary [%here] (Returns Value.Type.string) (fun () ->
          "hello world")))
    ~f:(fun () ->
      Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
        test_setup ~f:(fun clock ->
          profile
            Sync
            (lazy [%sexp "nest"])
            (fun () -> Clock.advance clock ~by:(sec 0.3)))));
  [%expect
    {|
    ((rendering_took 0us)
     (300_000us (nest "hello world") (19:00:00.000000000-05:00 1969-12-31))) |}];
  return ()
;;

let show_in_message value =
  test_setup ~f:(fun _ -> profile Sync (lazy [%sexp (value : Value.t)]) ignore)
;;

let%expect_test "[Value.sexp_of_t] respect [print_length] in profile records" =
  let test size =
    show_in_message (List.init size ~f:Fn.id |> Value.Type.(to_value (list int)))
  in
  test 10;
  [%expect
    {|
    ((rendering_took 0us)
     (0us (0 1 2 3 4 5 6 7 8 9 . ...) (19:00:00.000000000-05:00 1969-12-31))) |}];
  Customization.set_value_temporarily print_length (Some 3) ~f:(fun () -> test 10);
  [%expect
    {|
    ((rendering_took 0us)
     (0us (0 1 2 . ...) (19:00:00.000000000-05:00 1969-12-31))) |}];
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
     (0us (((((((nil) nil) (nil) nil) ((nil) nil) (nil) nil) (((nil) nil) (nil) nil) ((nil) nil) (nil) nil) ((((nil) nil) (nil) nil) ((nil) nil) (nil) nil) (((nil) nil) (nil) nil) ((nil) nil) (nil) nil) (((((nil) nil) (nil) nil) ((nil) nil) (nil) nil) (((nil) nil) (nil) nil) ((nil) nil) (nil) nil) ((((nil) nil) (nil) nil) ((nil) nil) (nil) nil) (((nil) nil) (nil) nil) ((nil) nil) (nil) nil) (19:00:00.000000000-05:00 1969-12-31))) |}];
  Customization.set_value_temporarily print_level (Some 3) ~f:(fun () ->
    show_in_message v7);
  [%expect
    {|
    ((rendering_took 0us)
     (0us (((... ... ... ... ...) (... ... ... ...) (... ... ...) (... ...) (...) nil) ((... ... ... ...) (... ... ...) (... ...) (...) nil) ((... ... ...) (... ...) (...) nil) ((... ...) (...) nil) ((...) nil) (nil) nil) (19:00:00.000000000-05:00 1969-12-31))) |}];
  return ()
;;
