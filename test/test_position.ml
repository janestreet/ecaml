open! Core_kernel
open! Async_kernel
open! Import
open! Position

let%expect_test "[to_byte_position], [of_byte_position]" =
  let show () =
    let position = Point.get () in
    let byte_position = position |> to_byte_position in
    let round_trip = byte_position |> of_byte_position in
    print_s
      [%message
        "" (position : Position.t) (byte_position : int) (round_trip : Position.t)];
    require_equal [%here] (module Position) position round_trip
  in
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    show ();
    [%expect
      {|
      ((position      1)
       (byte_position 1)
       (round_trip    1)) |}];
    Point.insert "a";
    show ();
    [%expect
      {|
      ((position      2)
       (byte_position 2)
       (round_trip    2)) |}];
    Point.insert "┌";
    show ();
    [%expect
      {|
      ((position      3)
       (byte_position 5)
       (round_trip    3)) |}]);
  return ()
;;
