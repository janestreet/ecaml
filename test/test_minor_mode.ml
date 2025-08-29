open! Core
open! Async_kernel
open! Import
open! Minor_mode

let%expect_test "[is_enabled], [enable], [disable]" =
  let is_enabled () = print_s [%sexp (is_enabled abbrev : bool)] in
  is_enabled ();
  [%expect {| false |}];
  enable abbrev;
  is_enabled ();
  [%expect {| true |}];
  disable abbrev;
  is_enabled ();
  [%expect {| false |}];
  return ()
;;

let%expect_test "[read_only]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    let is_enabled () = print_s [%sexp (is_enabled read_only : bool)] in
    is_enabled ();
    [%expect {| false |}];
    require_does_not_raise (fun () -> Point.insert "text");
    enable read_only;
    is_enabled ();
    [%expect {| true |}];
    require_does_raise (fun () -> Point.insert "text");
    [%expect {| (buffer-read-only ("#<buffer  *temp*>")) |}];
    disable read_only;
    is_enabled ();
    [%expect {| false |}];
    require_does_not_raise (fun () -> Point.insert "text"));
  return ()
;;

let%expect_test "custom minor mode, [temporarily_disable]" =
  let custom_minor_mode =
    define_minor_mode
      ("test-minor-mode" |> Symbol.intern)
      [%here]
      ~docstring:"Test"
      ~global:None
      ~initialize:(fun minor_mode ->
        print_s
          [%message
            "Minor mode initialization function called"
              ~enabled:(is_enabled minor_mode : bool)])
      ()
  in
  let is_enabled () = print_s [%sexp (is_enabled custom_minor_mode : bool)] in
  is_enabled ();
  [%expect {| false |}];
  enable custom_minor_mode;
  [%expect {| ("Minor mode initialization function called" (enabled true)) |}];
  is_enabled ();
  [%expect {| true |}];
  temporarily_disable Sync custom_minor_mode ~f:(fun () -> print_endline "f called");
  [%expect
    {|
    ("Minor mode initialization function called" (enabled false))
    f called
    ("Minor mode initialization function called" (enabled true))
    |}];
  disable custom_minor_mode;
  [%expect {| ("Minor mode initialization function called" (enabled false)) |}];
  is_enabled ();
  [%expect {| false |}];
  temporarily_disable Sync custom_minor_mode ~f:(fun () -> print_endline "f called");
  [%expect {| f called |}];
  return ()
;;
