open! Core
open! Async_kernel
open! Import
open Buffer_helper

let%expect_test "[show_point] does not modify buffer" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert "Foo bar baz.";
    Point.forward_char_exn (-3);
    let old = Current_buffer.chars_modified_tick () in
    show_point ();
    let new_ = Current_buffer.chars_modified_tick () in
    require_equal (module Modified_tick) old new_);
  [%expect {| Foo bar b█z. |}];
  return ()
;;

let%expect_test "[show_point] works at beginning and end of buffer." =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert "Foo bar baz.";
    Point.goto_min ();
    show_point ();
    Point.goto_max ();
    show_point ();
    (* Make sure we still delete the last char. *)
    Point.forward_char_exn (-1);
    show_point ());
  [%expect
    {|
    █oo bar baz.
    Foo bar baz.█
    Foo bar baz█
    |}];
  return ()
;;

let%expect_test "[show_active_region]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert "Foo bar baz.\nHam, spam, and eggs.";
    Point.goto_min ();
    show_point ();
    [%expect
      {|
      █oo bar baz.
      Ham, spam, and eggs.
      |}];
    Point.forward_char_exn 2;
    Current_buffer.set_mark (Point.get ());
    show_active_region ();
    [%expect {| "No region is active." |}];
    Point.forward_line_exn 1;
    Point.forward_char_exn 3;
    show_active_region ();
    [%expect
      {|
      Fo▛ bar baz.
      Ham▟ spam, and eggs.
      |}]);
  return ()
;;

let%expect_test "[show_point] works correctly when the buffer is narrowed" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert "Foo bar baz.\nSecond line.";
    Point.goto_min ();
    Point.forward_char_exn 7;
    show_point ();
    [%expect
      {|
      Foo bar█baz.
      Second line.
      |}];
    Current_buffer.narrow_to_region
      ~start:(Position.of_int_exn 5)
      ~end_:(Position.of_int_exn 20);
    (* Narrowing to a region that includes point doesn't move point, which remains on the
       space between "bar" and "baz". *)
    show_point ();
    [%expect
      {|
      <narrowed>
      bar█baz.
      Second
      <narrowed>
      |}];
    Point.goto_min ();
    show_point ();
    [%expect
      {|
      <narrowed>
      █ar baz.
      Second
      <narrowed>
      |}];
    Point.goto_max ();
    show_point ();
    [%expect
      {|
      <narrowed>
      bar baz.
      Second█
      <narrowed>
      |}]);
  return ()
;;
