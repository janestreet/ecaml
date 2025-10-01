open! Core
open! Async_kernel
open! Import
open! Tabulated_list

let test_mode = "test-tabulated-list-mode" |> Symbol.intern

type entry =
  { s1 : string
  ; i2 : int
  ; s3 : string
  }
[@@deriving fields ~getters, sexp_of]

let major_mode =
  define_derived_mode
    test_mode
    [%here]
    ~docstring:"for testing"
    ~mode_line:"Test-mode"
    ~parent:Tabulated_list.major_mode
    ()
;;

let t =
  let format =
    [ Column.create ~header:"s1" ~pad_right:3 s1
    ; Column.create ~header:"i2" ~sortable:false (fun x -> Int.to_string_hum (i2 x))
    ; Column.create ~header:"s3" ~align_right:true s3
    ]
  in
  create format ~get_id:s1
;;

let entries =
  [ { s1 = "b"; i2 = 1; s3 = "y" }
  ; { s1 = "a"; i2 = 2; s3 = "z" }
  ; { s1 = "c"; i2 = 3; s3 = "x" }
  ]
;;

let draw_and_print' t ?sort_by entries =
  let%bind () = Current_buffer.change_major_mode major_mode in
  draw ?sort_by t entries;
  printf "%s" (Current_buffer.contents () |> Text.to_utf8_bytes);
  return ()
;;

let draw_and_print = draw_and_print' t

(* The header is drawn in an overlay, and so is not part of the buffer contents. *)
let%expect_test "draw table" =
  Current_buffer.set_temporarily_to_temp_buffer Async (fun () ->
    let%bind () = draw_and_print entries in
    [%expect
      {|
      b    1   y
      a    2   z
      c    3   x
      |}];
    return ())
;;

let%expect_test "sort" =
  Current_buffer.set_temporarily_to_temp_buffer Async (fun () ->
    let%bind () = draw_and_print ~sort_by:("s1", `Ascending) entries in
    [%expect
      {|
      a    2   z
      b    1   y
      c    3   x
      |}];
    let%bind () = draw_and_print ~sort_by:("s1", `Descending) entries in
    [%expect
      {|
      c    3   x
      b    1   y
      a    2   z
      |}];
    let%bind () = draw_and_print ~sort_by:("s3", `Ascending) entries in
    [%expect
      {|
      c    3   x
      b    1   y
      a    2   z
      |}];
    let%bind () = draw_and_print ~sort_by:("s3", `Descending) entries in
    [%expect
      {|
      a    2   z
      b    1   y
      c    3   x
      |}];
    let%bind () =
      require_does_raise_async (fun () ->
        draw_and_print ~sort_by:("i2", `Ascending) entries)
    in
    [%expect {| ("Column is not sortable" i2) |}];
    return ())
;;

let%expect_test "id at point" =
  Current_buffer.set_temporarily_to_temp_buffer Async (fun () ->
    let%bind () = draw_and_print entries in
    [%expect
      {|
      b    1   y
      a    2   z
      c    3   x
      |}];
    Point.goto_min ();
    print_s [%sexp (get_record_at_point_exn t : entry option)];
    [%expect
      {|
      ((
        (s1 b)
        (i2 1)
        (s3 y)))
      |}];
    Point.forward_line 1;
    print_s [%sexp (get_record_at_point_exn t : entry option)];
    [%expect
      {|
      ((
        (s1 a)
        (i2 2)
        (s3 z)))
      |}];
    (* Point is preserved when list is redrawn. *)
    let%bind () = draw_and_print entries in
    [%expect
      {|
      b    1   y
      a    2   z
      c    3   x
      |}];
    print_s [%sexp (get_record_at_point_exn t : entry option)];
    [%expect
      {|
      ((
        (s1 a)
        (i2 2)
        (s3 z)))
      |}];
    Point.goto_max ();
    print_s [%sexp (get_record_at_point_exn t : entry option)];
    [%expect {| () |}];
    return ())
;;

let%expect_test "move_point_to_record" =
  Current_buffer.set_temporarily_to_temp_buffer Async (fun () ->
    let%bind () = draw_and_print entries in
    [%expect
      {|
      b    1   y
      a    2   z
      c    3   x
      |}];
    Tabulated_list.move_point_to_record t ~f:(s1 >> String.equal "a");
    print_s [%sexp (get_record_at_point_exn t : entry option)];
    [%expect
      {|
      ((
        (s1 a)
        (i2 2)
        (s3 z)))
      |}];
    Tabulated_list.move_point_to_record t ~f:(s1 >> String.equal "b");
    print_s [%sexp (get_record_at_point_exn t : entry option)];
    [%expect
      {|
      ((
        (s1 b)
        (i2 1)
        (s3 y)))
      |}];
    Tabulated_list.move_point_to_record t ~f:(s1 >> String.equal "c");
    print_s [%sexp (get_record_at_point_exn t : entry option)];
    [%expect
      {|
      ((
        (s1 c)
        (i2 3)
        (s3 x)))
      |}];
    return ())
;;

let%expect_test "generic sortable column" =
  let t_regular =
    create
      [ Column.create ~align_right:true ~header:"idx" ~sortable:true (fst >> Int.to_string)
      ; Column.create ~header:"file" ~sortable:true snd
      ]
      ~get_id:(fst >> Int.to_string)
  in
  let t_with_sort =
    create
      [ Column.create_gen
          ~align_right:true
          ~header:"idx"
          ~get_field:fst
          ~render_field:(Int.to_string >> Text.of_utf8_bytes)
          ~compare_field:Int.compare
          ()
      ; Column.create_gen
          ~header:"file"
          ~get_field:snd
          ~render_field:Text.of_utf8_bytes
          ~compare_field:
            (Comparable.lift
               (* sort by the number in the filename *)
               Int.compare
               ~f:
                 (String.chop_prefix_exn ~prefix:"file"
                  >> String.chop_suffix_exn ~suffix:".txt"
                  >> Int.of_string))
          ()
      ]
      ~get_id:(fst >> Int.to_string)
  in
  let data =
    [ 1; 2; 10; 11; 20 ] |> List.map ~f:(fun idx -> idx, sprintf "file%i.txt" idx)
  in
  let compare ~sort_by =
    print_endline ("Sorting by " ^ sort_by ^ "\n");
    let sort_by = sort_by, `Ascending in
    let draw_and_print t = draw_and_print' ~sort_by t data in
    print_endline "Columns with default emacs compare:";
    let%bind () = draw_and_print t_regular in
    print_endline "\nColumns with custom compare:";
    let%bind () = draw_and_print t_with_sort in
    return ()
  in
  let%bind () = compare ~sort_by:"idx" in
  [%expect
    {|
    Sorting by idx

    Columns with default emacs compare:
      1 file1.txt
     10 file10.txt
     11 file11.txt
      2 file2.txt
     20 file20.txt

    Columns with custom compare:
      1 file1.txt
      2 file2.txt
     10 file10.txt
     11 file11.txt
     20 file20.txt
    |}];
  let%bind () = compare ~sort_by:"file" in
  [%expect
    {|
    Sorting by file

    Columns with default emacs compare:
      1 file1.txt
     10 file10.txt
     11 file11.txt
      2 file2.txt
     20 file20.txt

    Columns with custom compare:
      1 file1.txt
      2 file2.txt
     10 file10.txt
     11 file11.txt
     20 file20.txt
    |}];
  return ()
;;
