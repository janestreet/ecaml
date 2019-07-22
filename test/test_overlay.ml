open! Core_kernel
open! Async_kernel
open! Import
open! Overlay

let with_buffer f =
  let buffer = Buffer.create ~name:"test-buffer" in
  Current_buffer.set_temporarily Sync buffer ~f:(fun () ->
    Point.insert
      {|
      Lorem ipsum dolor sit amet,
      consectetur adipiscing elit, sed
      doeiusmod tempor incididunt ut labore et dolore
      magna aliqua.|};
    f buffer)
;;

let count_at s position = print_s [%message "at" s ~_:(List.length (at position) : int)]

let count_in s ~start ~end_ =
  print_s [%message "in" s ~_:(List.length (in_ ~start ~end_) : int)]
;;

let num_overlays_in_buffer () =
  print_s [%sexp (List.length (in_ ~start:(Point.min ()) ~end_:(Point.max ())) : int)]
;;

let%expect_test "[create], [delete], [end_], [start]" =
  with_buffer (fun buffer ->
    let count_overlays () =
      count_at "point min" (Point.min ());
      count_in "buffer" ~start:(Point.min ()) ~end_:(Point.max ())
    in
    count_overlays ();
    [%expect {|
      (at "point min" 0)
      (in buffer 0) |}];
    let t = create () ~buffer ~start:(Point.min ()) ~end_:(Point.max ()) in
    count_overlays ();
    [%expect {|
      (at "point min" 1)
      (in buffer 1) |}];
    print_s [%message (start t : Position.t) (end_ t : Position.t)];
    print_s [%message (Point.min () : Position.t) (Point.max () : Position.t)];
    [%expect
      {|
      (("start t" 1)
       ("end_ t"  148))
      (("Point.min ()" 1)
       ("Point.max ()" 148)) |}];
    delete t;
    count_overlays ();
    [%expect {|
      (at "point min" 0)
      (in buffer 0) |}]);
  return ()
;;

let%expect_test "[move]" =
  with_buffer (fun _buffer ->
    let region1_start, region1_end = Position.of_int_exn 1, Position.of_int_exn 5 in
    let region2_start, region2_end = Position.of_int_exn 7, Position.of_int_exn 9 in
    let t = create () ~start:region1_start ~end_:region1_end in
    let count_overlays () =
      count_in "region1" ~start:region1_start ~end_:region1_end;
      count_in "region2" ~start:region2_start ~end_:region2_end
    in
    count_overlays ();
    [%expect {|
      (in region1 1)
      (in region2 0) |}];
    ignore (move t ~start:region2_start ~end_:region2_end : t);
    count_overlays ();
    [%expect {|
      (in region1 0)
      (in region2 1) |}]);
  return ()
;;

let%expect_test "[get_property], [put_property]" =
  with_buffer (fun _ ->
    let t = create () ~start:(Point.min ()) ~end_:(Point.max ()) in
    put_property t Text.Property_name.face [ Face Face.default ];
    print_s [%sexp (get_property t Text.Property_name.face : Text.Face_spec.t)];
    [%expect {| ((Face default)) |}]);
  return ()
;;

let%expect_test "[remove_overlays]" =
  with_buffer (fun _ ->
    remove_overlays ();
    num_overlays_in_buffer ();
    [%expect {| 0 |}];
    let add_overlay () =
      let t = create () ~start:(Point.min ()) ~end_:(Point.max ()) in
      put_property t Text.Property_name.face [ Face Face.default ]
    in
    add_overlay ();
    num_overlays_in_buffer ();
    [%expect {| 1 |}];
    remove_overlays ();
    num_overlays_in_buffer ();
    [%expect {| 0 |}];
    add_overlay ();
    remove_overlays
      ()
      ~with_property:(Text.Property_name.font_lock_face, [ Face Face.default ]);
    num_overlays_in_buffer ();
    [%expect {| 1 |}];
    remove_overlays () ~with_property:(Text.Property_name.face, [ Face Face.default ]);
    num_overlays_in_buffer ();
    [%expect {| 0 |}]);
  return ()
;;
