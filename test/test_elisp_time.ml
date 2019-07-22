open! Core
open! Async_kernel
open! Import
open! Elisp_time

let times =
  [ Time_ns.min_value_for_1us_rounding
  ; Time_ns.epoch
  ; Time_ns.of_string "2018-11-14 16:25:19.927203204-05:00"
  ; Time_ns.max_value_for_1us_rounding
  ]
;;

let%expect_test "[of_time_ns], [format], [to_time_ns_exn]" =
  List.iter times ~f:(fun time_ns ->
    let t = of_time_ns time_ns in
    let time_ns2 = to_time_ns_exn t in
    print_s [%message (time_ns : Time_ns.t) ~elisp_time:(t : t)];
    require_equal [%here] (module Time_ns) time_ns time_ns2);
  [%expect
    {|
    ((time_ns (1835-02-02 19:03:58.000000000-04:56:02))
     (elisp_time "1835-02-02 19:03:58.000000000-0456"))
    ((time_ns (1969-12-31 19:00:00.000000000-05:00))
     (elisp_time "1969-12-31 19:00:00.000000000-0500"))
    ((time_ns (2018-11-14 16:25:19.927203204-05:00))
     (elisp_time "2018-11-14 16:25:19.927203204-0500"))
    ((time_ns (2104-11-28 19:00:00.000000000-05:00))
     (elisp_time "2104-11-28 19:00:00.000000000-0500")) |}];
  return ()
;;
