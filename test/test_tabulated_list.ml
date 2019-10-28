open! Core_kernel
open! Async_kernel
open! Import
open! Tabulated_list

let test_mode = "test-tabulated-list-mode" |> Symbol.intern

type entry =
  { id : string
  ; s1 : string
  ; i2 : int
  ; s3 : string
  }
[@@deriving fields]

module M =
  (val define_derived_mode
         test_mode
         [%here]
         ~docstring:"for testing"
         ~mode_line:"Test-mode"
         ~parent:Tabulated_list_mode.major_mode
         ())

let t =
  let format =
    [ Column.create ~header:"s1" ~pad_right:3 s1
    ; Column.create ~header:"i2" ~sortable:false (fun x -> Int.to_string_hum (i2 x))
    ; Column.create ~header:"s3" ~align_right:true s3
    ]
  in
  create
    M.major_mode
    ~id_equal:String.equal
    ~id_of_record:id
    ~id_type:Value.Type.string
    format
;;

let entries =
  [ { id = "b"; s1 = "b"; i2 = 1; s3 = "y" }
  ; { id = "a"; s1 = "a"; i2 = 2; s3 = "z" }
  ; { id = "c"; s1 = "c"; i2 = 3; s3 = "x" }
  ]
;;

let draw_and_print ?sort_by entries =
  let%bind () = Current_buffer.change_major_mode (major_mode t) in
  draw ?sort_by t entries;
  printf "%s" (Current_buffer.contents () |> Text.to_utf8_bytes);
  return ()
;;

(* The header is drawn in an overlay, and so is not part of the buffer contents. *)
let%expect_test "draw table" =
  Current_buffer.set_temporarily_to_temp_buffer Async (fun () ->
    let%bind () = draw_and_print entries in
    [%expect {|
      b    1   y
      a    2   z
      c    3   x |}];
    return ())
;;

let%expect_test "sort" =
  Current_buffer.set_temporarily_to_temp_buffer Async (fun () ->
    let%bind () = draw_and_print ~sort_by:("s1", `Ascending) entries in
    [%expect {|
      a    2   z
      b    1   y
      c    3   x |}];
    let%bind () = draw_and_print ~sort_by:("s1", `Descending) entries in
    [%expect {|
      c    3   x
      b    1   y
      a    2   z |}];
    let%bind () = draw_and_print ~sort_by:("s3", `Ascending) entries in
    [%expect {|
      c    3   x
      b    1   y
      a    2   z |}];
    let%bind () = draw_and_print ~sort_by:("s3", `Descending) entries in
    [%expect {|
      a    2   z
      b    1   y
      c    3   x |}];
    let%bind () =
      require_does_raise_async [%here] (fun () ->
        draw_and_print ~sort_by:("i2", `Ascending) entries)
    in
    [%expect {|
      ("Column is not sortable" i2) |}];
    return ())
;;

let%expect_test "id at point" =
  Current_buffer.set_temporarily_to_temp_buffer Async (fun () ->
    let%bind () = draw_and_print entries in
    [%expect {|
      b    1   y
      a    2   z
      c    3   x |}];
    Point.goto_min ();
    print_s [%sexp (get_id_at_point_exn t : string option)];
    [%expect {| (b) |}];
    Point.forward_line 1;
    print_s [%sexp (get_id_at_point_exn t : string option)];
    [%expect {| (a) |}];
    Point.goto_max ();
    print_s [%sexp (get_id_at_point_exn t : string option)];
    [%expect {| () |}];
    return ())
;;

let%expect_test "move_point_to_id" =
  Current_buffer.set_temporarily_to_temp_buffer Async (fun () ->
    let%bind () = draw_and_print entries in
    [%expect {|
      b    1   y
      a    2   z
      c    3   x |}];
    Tabulated_list.move_point_to_id t "a";
    print_s [%sexp (get_id_at_point_exn t : string option)];
    [%expect {| (a) |}];
    Tabulated_list.move_point_to_id t "b";
    print_s [%sexp (get_id_at_point_exn t : string option)];
    [%expect {| (b) |}];
    Tabulated_list.move_point_to_id t "c";
    print_s [%sexp (get_id_at_point_exn t : string option)];
    [%expect {| (c) |}];
    return ())
;;
