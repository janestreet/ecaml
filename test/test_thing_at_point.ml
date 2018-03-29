open! Core_kernel
open! Import
open! Thing_at_point

let in_test_buffer f =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    Point.insert "This is/a test!buffer with\nvarious|things to match*against";
    Point.goto_min ();
    f ()
  )

let alpha_or_bang = String_of { chars = "[:alpha:]!" }
let number = String_of { chars = "[0-9]" }
let _ = number

let%expect_test "[find], [bounds]" =
  let test ~thing =
    let found = find thing in
    let bounds = bounds thing in
    let bounded_text =
      Option.map bounds ~f:(fun (start, end_) ->
        Current_buffer.contents ~start ~end_ ())
    in
    require_does_not_raise [%here] (fun () ->
      [%test_eq: Text.Compare_as_string.t option] found bounded_text);
    print_s [%sexp (found : Text.t option)]
  in

  in_test_buffer (fun () ->
    test ~thing:alpha_or_bang;
    [%expect {| (This) |}];
    test ~thing:number;
    [%expect {|
      () |}];
    Point.forward_char_exn 16; (* the "u" in "buffer" *)
    test ~thing:Word;
    [%expect {| (buffer) |}];
    test ~thing:alpha_or_bang;
    [%expect {| (test!buffer) |}];
  );
;;

let%expect_test "[forward], [beginning], [beginning_exn], [end_], [end_exn]" =
  let show_point () =
    let point = Point.get () in
    printf "%s│%s"
      (Current_buffer.contents ~end_: point () |> Text.to_utf8_bytes)
      (Current_buffer.contents ~start:point () |> Text.to_utf8_bytes)
  in
  in_test_buffer (fun () ->
    show_point ();
    [%expect {|
      │This is/a test!buffer with
      various|things to match*against |}];
    print_s [%sexp (forward Word : bool)];
    [%expect {| true |}];
    show_point ();
    [%expect {|
      This│ is/a test!buffer with
      various|things to match*against |}];
    print_s [%sexp (forward ~n:2 Word : bool)];
    show_point ();
    [%expect {|
      true
      This is/a│ test!buffer with
      various|things to match*against |}];
    (* Sadly, (forward-thing 'filename) is broken, at least as of GNU Emacs 25.3.1. *)
    show_raise (fun () ->
      ignore (forward alpha_or_bang : bool);
      show_point ());
    [%expect {| (raised ("Can't determine how to move over a filename")) |}];
    Point.forward_char_exn 1;
    end_exn Word;
    show_point();
    [%expect {|
      This is/a test│!buffer with
      various|things to match*against |}];
    print_s [%sexp (end_ Word : bool)];
    [%expect {|true|}];
    show_point();
    [%expect {|
      This is/a test│!buffer with
      various|things to match*against |}];
    beginning_exn Word;
    show_point();
    [%expect {|
      This is/a │test!buffer with
      various|things to match*against |}];
    end_exn alpha_or_bang;
    show_point();
    [%expect {|
      This is/a test!buffer│ with
      various|things to match*against |}];
    print_s [%sexp (beginning alpha_or_bang : bool)];
    [%expect {|true|}];
    show_point();
    [%expect {|
      This is/a │test!buffer with
      various|things to match*against |}];
    beginning_exn alpha_or_bang;
    show_point();
    [%expect {|
      This is/a │test!buffer with
      various|things to match*against |}];
  );
