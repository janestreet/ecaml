open! Core
open! Async_kernel
open! Import
open! Value

let format_time_string =
  Funcall.Wrap.("format-time-string" <: string @-> time_ns @-> value @-> return string)
;;

(* At least one example where the time value is constructed on the Lisp side.

   The last three arguments to encode-time just make it use UTC. *)
let constructed_in_lisp =
  let encode_time = Funcall.Wrap.("encode-time" <: list value @-> return time_ns) in
  let i = Value.of_int_exn in
  encode_time [ i 6; i 5; i 4; i 3; i 2; i 2021; Value.nil; i (-1); Value.t ]
;;

let examples =
  [ Time_ns.min_value_representable
  ; Time_ns.next Time_ns.min_value_representable
  ; Time_ns.prev Time_ns.max_value_representable
  ; Time_ns.max_value_representable
  ; Time_ns.epoch
  ; Time_ns.of_string "2024-05-31T14:51:40-04:00"
  ; constructed_in_lisp
  ]
;;

let%expect_test "times are semantically the same" =
  let format time = format_time_string "%F %T.%NZ" time Value.t in
  (let show time =
     let emacs_format = format time in
     let time_ns_format = Time_ns.to_string_utc time in
     let ocaml_parsing_emacs_format = Time_ns.of_string_with_utc_offset emacs_format in
     print_endline emacs_format;
     require_equal (module String) emacs_format time_ns_format;
     require_equal (module String) emacs_format (format ocaml_parsing_emacs_format)
   in
   List.iter examples ~f:show);
  [%expect
    {|
    1823-11-12 00:06:21.572612096Z
    1823-11-12 00:06:21.572612097Z
    2116-02-20 23:53:38.427387902Z
    2116-02-20 23:53:38.427387903Z
    1970-01-01 00:00:00.000000000Z
    2024-05-31 18:51:40.000000000Z
    2021-02-03 04:05:06.000000000Z
    |}];
  return ()
;;

let%expect_test "times roundtrip" =
  print_and_check_round_trip
    (module Time_ns)
    [ (module struct
        type t = Time_ns.t
        type repr = Value.t [@@deriving sexp_of]

        let repr_name = "Value.time_ns"
        let to_repr = Value.of_time_ns
        let of_repr = Value.to_time_ns_exn
      end)
    ]
    examples;
  [%expect
    {|
    (-4611686018427387904 . 1000000000)
    (-4611686018427387903 . 1000000000)
    (4611686018427387902 . 1000000000)
    (4611686018427387903 . 1000000000)
    (0 . 1000000000)
    (1717181500000000000 . 1000000000)
    (1612325106000000000 . 1000000000)
    |}];
  return ()
;;
