open! Core
open! Async_kernel
open! Import
open! Position

let%expect_test "[to_byte_position_exn], [of_byte_position_exn]" =
  let show () =
    let position = Point.get () in
    let byte_position = position |> to_byte_position_exn in
    let round_trip = byte_position |> of_byte_position_exn in
    print_s
      [%message
        "" (position : Position.t) (byte_position : int) (round_trip : Position.t)];
    require_equal (module Position) position round_trip
  in
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    show ();
    [%expect
      {|
      ((position      1)
       (byte_position 1)
       (round_trip    1))
      |}];
    Point.insert "a";
    show ();
    [%expect
      {|
      ((position      2)
       (byte_position 2)
       (round_trip    2))
      |}];
    Point.insert "┌";
    show ();
    [%expect
      {|
      ((position      3)
       (byte_position 5)
       (round_trip    3))
      |}];
    require_does_raise (fun () -> Point.get () |> add _ 1 |> to_byte_position_exn);
    [%expect {| ("Position out of range" (pos 4)) |}];
    let invalid_byte_position = Point.get () |> to_byte_position_exn |> Int.succ in
    require_does_raise (fun () -> invalid_byte_position |> of_byte_position_exn);
    [%expect {| ("Byte position out of range" (bpos 6)) |}];
    require_does_raise (fun () -> of_byte_position_exn 0);
    [%expect {| ("Byte position out of range" (bpos 0)) |}];
    require_does_raise (fun () -> to_byte_position_exn (of_int_exn 0));
    [%expect {| ("Position out of range" (pos 0)) |}]);
  return ()
;;
