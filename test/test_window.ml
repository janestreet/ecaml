open! Core_kernel
open! Import
open! Window

let show_all () = print_s [%sexp (all_in_selected_frame () : t list)]

let%expect_test "[all_in_selected_frame]" =
  show_all ();
  [%expect {|
    ("#<window 1 on *scratch*>") |}];
;;

let try_with = Or_error.try_with

let show t =
  print_s [%message
    ""
      ~body_height: (try_with (fun () -> body_height_exn t) : int        Or_error.t)
      ~buffer:      (try_with (fun () -> buffer_exn      t) : Buffer.t   Or_error.t)
      ~height:      (try_with (fun () -> height_exn      t) : int        Or_error.t)
      ~is_live:     (try_with (fun () -> is_live         t) : bool       Or_error.t)
      ~point:       (try_with (fun () -> point_exn       t) : Position.t Or_error.t)
      ~width:       (try_with (fun () -> width_exn       t) : int        Or_error.t)]
;;

let t = List.hd_exn (all_in_selected_frame ())

let%expect_test "accessors" =
  show t;
  [%expect {|
    ((body_height (Ok 8))
     (buffer      (Ok "#<buffer *scratch*>"))
     (height      (Ok 9))
     (is_live     (Ok true))
     (point       (Ok 1))
     (width       (Ok 10))) |}];
;;

let%expect_test "[set_point_exn]" =
  Point.insert "foo";
  set_point_exn t (2 |> Position.of_int_exn);
  print_s [%sexp (point_exn t : Position.t)];
  [%expect {|
    2 |}];
;;

let%expect_test "[delete_exn] raise" =
  show_raise (fun () -> delete_exn t);
  [%expect {|
    (raised ("Attempt to delete minibuffer or sole ordinary window")) |}]
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
  [%expect {|
    ((body_height (Error (wrong-type-argument (window-live-p #<window 1>))))
     (buffer (Error ("[buffer]'s [of_value_exn] got value not in subtype" nil)))
     (height (Error (wrong-type-argument (window-valid-p #<window 1>))))
     (is_live (Ok false))
     (point (Error (wrong-type-argument (window-live-p #<window 1>))))
     (width (Error (wrong-type-argument (window-live-p #<window 1>))))) |}]
;;
