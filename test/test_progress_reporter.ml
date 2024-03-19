open! Core
open! Async_kernel
open! Import

let%expect_test "spinner" =
  let spinner =
    Progress_reporter.create_spinner_and_display ~message:"watering flowers..."
  in
  [%expect {| watering flowers... \ |}];
  Progress_reporter.update spinner ();
  Progress_reporter.update spinner ~suffix:"tulips" ();
  Progress_reporter.update spinner ~suffix:"roses" ();
  Progress_reporter.finish spinner;
  [%expect
    {|
    watering flowers... |
    watering flowers... / tulips
    watering flowers... - roses
    watering flowers...done
    |}];
  return ()
;;

let%expect_test "range" =
  let reporter =
    Progress_reporter.create_with_range_and_display
      ~min_incl:0
      ~max_incl:3
      ~message:"watering flowers..."
  in
  [%expect {| watering flowers... |}];
  Progress_reporter.update reporter 1;
  Progress_reporter.update reporter ~suffix:"tulips" 2;
  Progress_reporter.update reporter ~suffix:"roses" 3;
  Progress_reporter.finish reporter;
  [%expect
    {|
    watering flowers...33%
    watering flowers...66% tulips
    watering flowers...100% roses
    watering flowers...done
    |}];
  return ()
;;

let%expect_test "list" =
  let%bind () =
    Progress_reporter.Deferred.List.iter
      ~message:"watering flowers..."
      (List.range 0 10)
      ~how:`Sequential
      ~f:(fun _ -> return ())
      ~suffix:(fun n -> sprintf "#%d" n)
  in
  [%expect
    {|
    watering flowers...
    watering flowers...10% #0
    watering flowers...20% #1
    watering flowers...30% #2
    watering flowers...40% #3
    watering flowers...50% #4
    watering flowers...60% #5
    watering flowers...70% #6
    watering flowers...80% #7
    watering flowers...90% #8
    watering flowers...100% #9
    watering flowers...done
    |}];
  return ()
;;
