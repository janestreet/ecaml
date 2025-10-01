open! Core
open! Async_kernel
open! Import
open! Comment
open! Ecaml_test_helpers

let mode lang = Funcall.Wrap.(lang ^ "-mode" <: nullary @-> return nil) ()

let%expect_test "options" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    mode "c";
    let show var sexp_of_a = print_s [%sexp (var () : a)] in
    show start String.sexp_of_t;
    [%expect {| "/* " |}];
    show end_ String.sexp_of_t;
    [%expect {| " */" |}];
    show multi_line Bool.sexp_of_t;
    [%expect {| true |}];
    return ())
;;

let test ?position () =
  let position =
    Option.value_map position ~f:Position.of_int_exn ~default:(Point.get ())
  in
  let one_fn name f =
    print_endline [%string "%{name}:"];
    Point.goto_char position;
    Ecaml_test_helpers.show ();
    print_endline "=>";
    match f () with
    | () -> Ecaml_test_helpers.show ()
    | exception exn -> print_s [%sexp "raised", (exn : exn)]
  in
  one_fn "goto_end_exn" goto_end_exn;
  print_endline "";
  one_fn "goto_beginning_exn" goto_beginning_exn;
  print_endline "\ncomment bounds:";
  Point.goto_char position;
  match bounds_of_comment_at_point () with
  | None -> print_endline "  None"
  | Some (start, end_) ->
    Buffer_helper.show_buffer
      ~block_out:
        [ start, Buffer_helper.utf8_upper_left_U259B
        ; end_, Buffer_helper.utf8_lower_right_U259F
        ]
;;

let%expect_test "goto_*_exn on multiline comment" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    mode "html";
    Point.insert
      {|
<!-- this is a comment
     it spans multiple lines
     end of the comment -->
|};
    ();
    test ();
    [%expect
      {|
      goto_end_exn:

      <!-- this is a comment
           it spans multiple lines
           end of the comment -->
      █
      =>
      (raised "not in a comment")

      goto_beginning_exn:

      <!-- this is a comment
           it spans multiple lines
           end of the comment -->
      █
      =>
      (raised "not in a comment")

      comment bounds:
        None
      |}];
    test ~position:3 ();
    [%expect
      {|
      goto_end_exn:

      <█-- this is a comment
           it spans multiple lines
           end of the comment -->

      =>

      <!-- this is a comment
           it spans multiple lines
           end of the comment█-->


      goto_beginning_exn:

      <█-- this is a comment
           it spans multiple lines
           end of the comment -->

      =>

      <!-- █his is a comment
           it spans multiple lines
           end of the comment -->


      comment bounds:

      ▛!-- this is a comment
           it spans multiple lines
           end of the comment -->▟
      |}];
    test ~position:30 ();
    [%expect
      {|
      goto_end_exn:

      <!-- this is a comment
           █t spans multiple lines
           end of the comment -->

      =>

      <!-- this is a comment
           it spans multiple lines
           end of the comment█-->


      goto_beginning_exn:

      <!-- this is a comment
           █t spans multiple lines
           end of the comment -->

      =>

      <!-- █his is a comment
           it spans multiple lines
           end of the comment -->


      comment bounds:

      ▛!-- this is a comment
           it spans multiple lines
           end of the comment -->▟
      |}];
    test ~position:52 ();
    [%expect
      {|
      goto_end_exn:

      <!-- this is a comment
           it spans multiple line█
           end of the comment -->

      =>

      <!-- this is a comment
           it spans multiple lines
           end of the comment█-->


      goto_beginning_exn:

      <!-- this is a comment
           it spans multiple line█
           end of the comment -->

      =>

      <!-- █his is a comment
           it spans multiple lines
           end of the comment -->


      comment bounds:

      ▛!-- this is a comment
           it spans multiple lines
           end of the comment -->▟
      |}];
    return ())
;;

let%expect_test "goto_end_exn on single-line comment" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    mode "org";
    Point.insert
      {|
# this is a comment
# it spans multiple lines
# end of the comment
|};
    ();
    test ();
    [%expect
      {|
      goto_end_exn:

      # this is a comment
      # it spans multiple lines
      # end of the comment
      █
      =>
      (raised "not in a comment")

      goto_beginning_exn:

      # this is a comment
      # it spans multiple lines
      # end of the comment
      █
      =>
      (raised "not in a comment")

      comment bounds:
        None
      |}];
    test ~position:3 ();
    [%expect
      {|
      goto_end_exn:

      #█this is a comment
      # it spans multiple lines
      # end of the comment

      =>

      # this is a comment
      # it spans multiple lines
      # end of the comment█


      goto_beginning_exn:

      #█this is a comment
      # it spans multiple lines
      # end of the comment

      =>

      # █his is a comment
      # it spans multiple lines
      # end of the comment


      comment bounds:

      # ▛his is a comment
      # it spans multiple lines
      # end of the comment▟
      |}];
    test ~position:30 ();
    [%expect
      {|
      goto_end_exn:

      # this is a comment
      # it spa█s multiple lines
      # end of the comment

      =>

      # this is a comment
      # it spans multiple lines
      # end of the comment█


      goto_beginning_exn:

      # this is a comment
      # it spa█s multiple lines
      # end of the comment

      =>

      # █his is a comment
      # it spans multiple lines
      # end of the comment


      comment bounds:

      # ▛his is a comment
      # it spans multiple lines
      # end of the comment▟
      |}];
    test ~position:52 ();
    [%expect
      {|
      goto_end_exn:

      # this is a comment
      # it spans multiple lines
      # en█ of the comment

      =>

      # this is a comment
      # it spans multiple lines
      # end of the comment█


      goto_beginning_exn:

      # this is a comment
      # it spans multiple lines
      # en█ of the comment

      =>

      # █his is a comment
      # it spans multiple lines
      # end of the comment


      comment bounds:

      # ▛his is a comment
      # it spans multiple lines
      # end of the comment▟
      |}];
    return ())
;;

let%expect_test "single-line-comments on the same line as code" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    mode "python";
    [%expect {| Can't guess python-indent-offset, using defaults: 4 |}];
    Point.insert
      {|
def hello():
  print("Hello, world")      # this is a comment
  return 3                   # is this the same comment?
|};
    ();
    test ~position:45 ();
    [%expect
      {|
      goto_end_exn:

      def hello():
        print("Hello, world")      #█this is a comment
        return 3                   # is this the same comment?

      =>

      def hello():
        print("Hello, world")      # this is a comment
        return 3                   # is this the same comment?█


      goto_beginning_exn:

      def hello():
        print("Hello, world")      #█this is a comment
        return 3                   # is this the same comment?

      =>

      def hello():
        print("Hello, world")      # █his is a comment
        return 3                   # is this the same comment?


      comment bounds:

      def hello():
        print("Hello, world")      # ▛his is a comment
        return 3                   # is this the same comment?▟
      |}];
    return ())
;;

let%expect_test "two comments on one line" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    mode "html";
    Point.insert
      {|
<!-- comment 1 -->  <!-- comment 2 -->
|};
    test ~position:5 ();
    [%expect
      {|
      goto_end_exn:

      <!-█ comment 1 -->  <!-- comment 2 -->

      =>

      <!-- comment 1█-->  <!-- comment 2 -->


      goto_beginning_exn:

      <!-█ comment 1 -->  <!-- comment 2 -->

      =>

      <!-- █omment 1 -->  <!-- comment 2 -->


      comment bounds:

      ▛!-- comment 1 -->▟ <!-- comment 2 -->
      |}];
    test ~position:21 ();
    [%expect
      {|
      goto_end_exn:

      <!-- comment 1 --> █<!-- comment 2 -->

      =>
      (raised "not in a comment")

      goto_beginning_exn:

      <!-- comment 1 --> █<!-- comment 2 -->

      =>
      (raised "not in a comment")

      comment bounds:
        None
      |}];
    test ~position:35 ();
    [%expect
      {|
      goto_end_exn:

      <!-- comment 1 -->  <!-- comment █ -->

      =>

      <!-- comment 1 -->  <!-- comment 2█-->


      goto_beginning_exn:

      <!-- comment 1 -->  <!-- comment █ -->

      =>

      <!-- comment 1 -->  <!-- █omment 2 -->


      comment bounds:

      <!-- comment 1 -->  ▛!-- comment 2 -->▟
      |}];
    return ())
;;
