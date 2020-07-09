open! Core_kernel
open! Async_kernel
open! Import
open! Comment

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

let%expect_test "goto_end_exn on multiline comment" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    mode "html";
    Point.insert
      {|
<!-- this is a comment
     it spans multiple lines
     end of the comment -->
|};
    ();
    let test ?position () =
      Option.iter position ~f:(Point.goto_char << Position.of_int_exn);
      Ecaml_test_helpers.show ();
      print_endline "=>";
      match goto_end_exn () with
      | () -> Ecaml_test_helpers.show ()
      | exception exn -> print_s [%sexp "raised", (exn : exn)]
    in
    test ();
    [%expect
      {|
      <!-- this is a comment
           it spans multiple lines
           end of the comment -->
      █
      =>
      (raised ("Could not find end of comment")) |}];
    test ~position:3 ();
    [%expect
      {|
      <█-- this is a comment
           it spans multiple lines
           end of the comment -->

      =>

      <!-- this is a comment
           it spans multiple lines
           end of the comment█--> |}];
    test ~position:30 ();
    [%expect
      {|
      <!-- this is a comment
           █t spans multiple lines
           end of the comment -->

      =>

      <!-- this is a comment
           it spans multiple lines
           end of the comment█--> |}];
    test ~position:52 ();
    [%expect
      {|
      <!-- this is a comment
           it spans multiple line█
           end of the comment -->

      =>

      <!-- this is a comment
           it spans multiple lines
           end of the comment█--> |}];
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
    let test ?position () =
      Option.iter position ~f:(Point.goto_char << Position.of_int_exn);
      Ecaml_test_helpers.show ();
      print_endline "=>";
      match goto_end_exn () with
      | () -> Ecaml_test_helpers.show ()
      | exception exn -> print_s [%sexp "raised", (exn : exn)]
    in
    test ();
    [%expect
      {|
      # this is a comment
      # it spans multiple lines
      # end of the comment
      █
      =>

      # this is a comment
      # it spans multiple lines
      # end of the comment█ |}];
    test ~position:3 ();
    [%expect
      {|
      #█this is a comment
      # it spans multiple lines
      # end of the comment

      =>

      # this is a comment
      # it spans multiple lines
      # end of the comment█ |}];
    test ~position:30 ();
    [%expect
      {|
      # this is a comment
      # it spa█s multiple lines
      # end of the comment

      =>

      # this is a comment
      # it spans multiple lines
      # end of the comment█ |}];
    test ~position:52 ();
    [%expect
      {|
      # this is a comment
      # it spans multiple lines
      # en█ of the comment

      =>

      # this is a comment
      # it spans multiple lines
      # end of the comment█ |}];
    return ())
;;
