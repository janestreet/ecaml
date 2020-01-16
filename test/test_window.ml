open! Core_kernel
open! Async_kernel
open! Import
open! Window

let show_all () = print_s [%sexp (Frame.window_list () : t list)]

let%expect_test "[all_in_selected_frame]" =
  show_all ();
  [%expect {| ("#<window 1 on *scratch*>") |}];
  return ()
;;

let try_with = Or_error.try_with

let show t =
  print_s
    [%message
      ""
        ~body_height:(try_with (fun () -> body_height_exn t) : int Or_error.t)
        ~buffer:(try_with (fun () -> buffer_exn t) : Buffer.t Or_error.t)
        ~height:(try_with (fun () -> height_exn t) : int Or_error.t)
        ~is_live:(try_with (fun () -> is_live t) : bool Or_error.t)
        ~point:(try_with (fun () -> point_exn t) : Position.t Or_error.t)
        ~width:(try_with (fun () -> width_exn t) : int Or_error.t)]
;;

let t = List.hd_exn (Frame.window_list ())

let%expect_test "accessors" =
  show t;
  [%expect
    {|
    ((body_height (Ok 8))
     (buffer      (Ok "#<buffer *scratch*>"))
     (height      (Ok 9))
     (is_live     (Ok true))
     (point       (Ok 1))
     (width       (Ok 10))) |}];
  return ()
;;

let%expect_test "[set_point_exn]" =
  Point.insert "foo";
  set_point_exn t (2 |> Position.of_int_exn);
  print_s [%sexp (point_exn t : Position.t)];
  [%expect {| 2 |}];
  return ()
;;

let%expect_test "[delete_exn] raise" =
  show_raise (fun () -> delete_exn t);
  [%expect {| (raised ("Attempt to delete minibuffer or sole ordinary window")) |}];
  return ()
;;

let%expect_test "[delete_exn]" =
  Selected_window.split_vertically_exn ();
  show_all ();
  [%expect {|
    ("#<window 1 on *scratch*>" "#<window 4 on *scratch*>") |}];
  delete_exn t;
  show_all ();
  [%expect {|
    ("#<window 4 on *scratch*>") |}];
  show t;
  [%expect
    {|
    ((body_height (Error (wrong-type-argument (window-live-p "#<window 1>"))))
     (buffer (Error (Window.buffer_exn "#<window 1>")))
     (height (Error (wrong-type-argument (window-valid-p "#<window 1>"))))
     (is_live (Ok false))
     (point (Error (wrong-type-argument (window-live-p "#<window 1>"))))
     (width (Error (wrong-type-argument (window-live-p "#<window 1>"))))) |}];
  return ()
;;

let%expect_test "[Tree]" =
  let show () =
    let window_tree = Frame.window_tree (frame (Selected_window.get ())) in
    print_s [%sexp (window_tree : Window.Tree.t)];
    List.iter (Frame.window_list ()) ~f:show
  in
  show ();
  [%expect
    {|
    (Window "#<window 4 on *scratch*>")
    ((body_height (Ok 8))
     (buffer      (Ok "#<buffer *scratch*>"))
     (height      (Ok 9))
     (is_live     (Ok true))
     (point       (Ok 2))
     (width       (Ok 10))) |}];
  Selected_window.split_vertically_exn ();
  show ();
  [%expect
    {|
    (Combination
      (children (
        (Window "#<window 4 on *scratch*>")
        (Window "#<window 6 on *scratch*>")))
      (direction Top_to_bottom)
      (edges (
        (bottom 10)
        (left   0)
        (right  10)
        (top    1))))
    ((body_height (Ok 4))
     (buffer      (Ok "#<buffer *scratch*>"))
     (height      (Ok 5))
     (is_live     (Ok true))
     (point       (Ok 2))
     (width       (Ok 10)))
    ((body_height (Ok 3))
     (buffer      (Ok "#<buffer *scratch*>"))
     (height      (Ok 4))
     (is_live     (Ok true))
     (point       (Ok 2))
     (width       (Ok 10))) |}];
  return ()
;;
