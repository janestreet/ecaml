open! Core_kernel
open! Import
open! Marker

let show t =
  print_s [%message
    ""
      ~marker:(t : t)
      ~buffer:(buffer t : Buffer.t option)
      ~insertion_type:(insertion_type t : Insertion_type.t)
      ~position:(position t : Position.t option)]
;;

let%expect_test "[create], accessors" =
  show (create ());
  [%expect {|
    ((marker "#<marker in no buffer>")
     (buffer ())
     (insertion_type Before_inserted_text)
     (position ())) |}];
;;

let%expect_test "[set_insertion_type]" =
  let t = create () in
  set_insertion_type t After_inserted_text;
  show t;
  [%expect {|
    ((marker "#<marker (moves after insertion) in no buffer>")
     (buffer ())
     (insertion_type After_inserted_text)
     (position ())) |}];
  set_insertion_type t Before_inserted_text;
  show t;
  [%expect {|
    ((marker "#<marker in no buffer>")
     (buffer ())
     (insertion_type Before_inserted_text)
     (position ())) |}];
;;

let%expect_test "[set]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    let t = create () in
    Point.insert "foo";
    set t (Current_buffer.get ()) (2 |> Position.of_int_exn);
    show t;
    [%expect {|
      ((marker "#<marker at 2 in *temp-buffer*>")
       (buffer ("#<buffer *temp-buffer*>"))
       (insertion_type Before_inserted_text)
       (position (2))) |}];
    Point.goto_char (Point.min ());
    Point.insert "bar";
    show t;
    [%expect {|
      ((marker "#<marker at 5 in *temp-buffer*>")
       (buffer ("#<buffer *temp-buffer*>"))
       (insertion_type Before_inserted_text)
       (position (5))) |}]);
;;

let%expect_test "[copy]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    let t1 = create () in
    let t2 = copy t1 in
    Point.insert "foo";
    show t2;
    [%expect {|
      ((marker "#<marker in no buffer>")
       (buffer ())
       (insertion_type Before_inserted_text)
       (position ())) |}];
    Current_buffer.set_marker_position t1 (2 |> Position.of_int_exn);
    show t1;
    [%expect {|
      ((marker "#<marker at 2 in *temp-buffer*>")
       (buffer ("#<buffer *temp-buffer*>"))
       (insertion_type Before_inserted_text)
       (position (2))) |}];
    show t2;
    [%expect {|
      ((marker "#<marker in no buffer>")
       (buffer ())
       (insertion_type Before_inserted_text)
       (position ())) |}]);
;;
