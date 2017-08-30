open! Core_kernel
open! Import
open! Buffer

let show t = print_s [%sexp (t : t)]

let num_live () = List.length (all_live ())

let num_live_at_start = num_live ()

let%expect_test "[all_live]" =
  print_s [%sexp (all_live () : t list)];
  [%expect {|
    ("#<buffer *scratch*>"
     "#<buffer  *Minibuf-0*>"
     "#<buffer *Messages*>"
     "#<buffer  *code-conversion-work*>") |}];
;;

let require_clean () =
  require [%here] (num_live () = num_live_at_start);
;;

let%expect_test "[create]" =
  let t1 = create ~name:"foo" in
  show t1;
  [%expect {|
    "#<buffer foo>" |}];
  let t2 = create ~name:"foo" in
  show t2;
  [%expect {|
    "#<buffer foo<2>>" |}];
  kill t1;
  kill t2;
  require_clean ();
;;

let%expect_test "[kill]" =
  let t = find_or_create ~name:"test-buffer" in
  kill t;
  show t;
  [%expect {|
    "#<killed buffer>" |}];
  require_clean ();
;;

let%expect_test "[equal]" =
  let t1 = create ~name:"1" in
  let t2 = create ~name:"2" in
  require [%here] (equal t1 t1);
  require [%here] (not (equal t1 t2));
  kill t1;
  kill t2;
  require_clean ();
;;

let%expect_test "[name]" =
  let t = create ~name:"some-name" in
  print_s [%sexp (name t : string option)];
  [%expect {|
    (some-name) |}];
  kill t;
  print_s [%sexp (name t : string option)];
  [%expect {|
    () |}];
  require_clean ();
;;

let%expect_test "[file_name]" =
  let t = create ~name:"some-name" in
  print_s [%sexp (file_name t : string option)];
  [%expect {|
    () |}];
  kill t;
  print_s [%sexp (file_name t : string option)];
  [%expect {|
    () |}];
  require_clean ();
;;

let%expect_test "[is_live]" =
  let t = find_or_create ~name:"test-buffer" in
  let show () = print_s [%message "" ~is_live:(is_live t : bool)] in
  show ();
  [%expect {|
    (is_live true) |}];
  kill t;
  show ();
  [%expect {|
    (is_live false) |}];
  require_clean ();
;;

let%expect_test "[find] Some" =
  let t = create ~name:"foo" in
  print_s [%sexp (find ~name:"foo" : t option)];
  [%expect {|
    ("#<buffer foo>") |}];
  kill t;
  require_clean ();
;;

let%expect_test "[find] None" =
  print_s [%sexp (find ~name:"no buffer with this name" : t option)];
  [%expect {|
    () |}];
  require_clean ();
;;

let%expect_test "[find_or_create]" =
  let f () = find_or_create ~name:"test-buffer" in
  let t = f () in
  show t;
  [%expect {|
    "#<buffer test-buffer>" |}];
  require [%here] (equal t (f ()));
  kill t;
  require_clean ();
;;

let%expect_test "[displayed_in]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    let t = Current_buffer.get () in
    let show_displayed_in () = print_s [%sexp (displayed_in t : Window.t list)] in
    show_displayed_in ();
    [%expect {|
      () |}];
    Selected_window.switch_to_buffer t;
    show_displayed_in ();
    [%expect {|
      ("#<window 1 on *temp-buffer*>") |}];
    Selected_window.split_vertically_exn ();
    show_displayed_in ();
    [%expect {|
      ("#<window 1 on *temp-buffer*>" "#<window 4 on *temp-buffer*>") |}]);
;;

let%expect_test "[display]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    let t = Current_buffer.get () in
    let show_displayed_in () = print_s [%sexp (displayed_in t : Window.t list)] in
    show_displayed_in ();
    [%expect {|
      () |}];
    display t;
    show_displayed_in ();
    [%expect {|
      ("#<window 4 on *temp-buffer*>") |}]);
;;
