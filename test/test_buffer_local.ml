open! Core_kernel
open! Async_kernel
open! Import
open! Buffer_local

let int =
  defvar
    ("some-int" |> Symbol.intern)
    [%here]
    ~type_:Value.Type.(option int)
    ~default_value:None
    ()
;;

let%expect_test "[symbol]" =
  print_s [%sexp (symbol int : Symbol.t)];
  [%expect {| some-int |}];
  return ()
;;

let show_in_current_buffer () =
  print_s [%sexp (Current_buffer.get_buffer_local int : int option)]
;;

let%expect_test "[get] with no value" =
  show_in_current_buffer ();
  [%expect {|
    () |}];
  require_does_raise [%here] (fun () -> Current_buffer.get_buffer_local_exn int);
  [%expect
    {|
    ("buffer has no value for variable"
      (variable (some-int (option int)))
      (buffer "#<buffer *scratch*>")) |}];
  return ()
;;

let%expect_test "[get] with some value" =
  Current_buffer.set_buffer_local int (Some 13);
  show_in_current_buffer ();
  [%expect {| (13) |}];
  print_s [%sexp (Current_buffer.get_buffer_local_exn int : int)];
  [%expect {| 13 |}];
  return ()
;;

let%expect_test "[get] with strange value" =
  Current_buffer.set_value (Var.create (symbol int) Value.Type.string) "thirteen";
  require_does_raise [%here] (fun () -> Current_buffer.get_buffer_local int);
  [%expect
    {|
    ("buffer has strange value for variable"
      (variable (some-int (option int)))
      (buffer "#<buffer *scratch*>")
      (value  thirteen)) |}];
  require_does_raise [%here] (fun () -> Current_buffer.get_buffer_local_exn int);
  [%expect
    {|
    ("buffer has strange value for variable"
      (variable (some-int (option int)))
      (buffer "#<buffer *scratch*>")
      (value  thirteen)) |}];
  return ()
;;

let%expect_test "different values in different buffers" =
  let b1 = Buffer.create ~name:"b1" in
  let b2 = Buffer.create ~name:"b2" in
  set int (Some 1) b1;
  set int (Some 2) b2;
  print_s [%sexp (get_exn int b1 : int)];
  [%expect {| 1 |}];
  print_s [%sexp (get_exn int b2 : int)];
  [%expect {| 2 |}];
  return ()
;;

let%expect_test "[get] with value represented as [nil]" =
  let bool =
    defvar
      ("some-bool" |> Symbol.intern)
      [%here]
      ~type_:Value.Type.(option bool)
      ~default_value:None
      ()
  in
  let test b =
    Current_buffer.set_buffer_local bool (Some b);
    print_s [%sexp (Current_buffer.get_buffer_local_exn bool : bool)]
  in
  test false;
  [%expect {| false |}];
  test true;
  [%expect {| true |}];
  return ()
;;

let%expect_test "[defvar_embedded]" =
  let t =
    defvar_embedded
      ("x" |> Symbol.intern)
      [%here]
      (module struct
        type t = int ref [@@deriving sexp_of]
      end)
  in
  let show () = print_s [%sexp (Current_buffer.get_buffer_local t : int ref option)] in
  show ();
  [%expect {| () |}];
  let r = ref 13 in
  Current_buffer.set_buffer_local t (Some r);
  show ();
  [%expect {| (13) |}];
  r := 14;
  show ();
  [%expect {| (14) |}];
  return ()
;;

let%expect_test "[defvar ~wrapped:false]" =
  let t =
    defvar
      ("unwrapped" |> Symbol.intern)
      [%here]
      ~type_:Value.Type.(nil_or int)
      ~default_value:None
      ()
  in
  let show () = print_s [%sexp (Current_buffer.get_buffer_local t : int option)] in
  show ();
  [%expect {| () |}];
  Current_buffer.set_buffer_local t (Some 13);
  show ();
  [%expect {| (13) |}];
  return ()
;;

let%expect_test "non-nil default value" =
  let t =
    defvar
      ("non-nil-default" |> Symbol.intern)
      [%here]
      ~type_:Value.Type.int
      ~default_value:13
      ()
  in
  let show () = print_s [%sexp (Current_buffer.get_buffer_local t : int)] in
  show ();
  [%expect {| 13 |}];
  Current_buffer.set_buffer_local t 14;
  show ();
  [%expect {| 14 |}];
  return ()
;;

let%expect_test "[wrap_existing]" =
  let var =
    Defvar.defvar
      ("for-wrapping" |> Symbol.intern)
      [%here]
      ~docstring:""
      ~type_:Value.Type.string
      ~initial_value:"initial"
      ()
  in
  let wrap ~make_buffer_local_always =
    wrap_existing var.symbol var.type_ ~make_buffer_local_always
  in
  print_s [%sexp (Var.is_buffer_local_always var : bool)];
  [%expect {| false |}];
  require_does_raise [%here] (fun () -> wrap ~make_buffer_local_always:false);
  [%expect
    {|
    ("[Buffer_local.wrap_existing] on an Elisp variable that is not automatically buffer local"
     (symbol for-wrapping)) |}];
  let (_ : string t) = wrap ~make_buffer_local_always:true in
  print_s [%sexp (Var.is_buffer_local_always var : bool)];
  [%expect {| true |}];
  return ()
;;

let%expect_test "non-permanent buffer-locals cleared on changing major modes" =
  Current_buffer.set_temporarily_to_temp_buffer Async (fun () ->
    Current_buffer.set_buffer_local int (Some 23);
    show_in_current_buffer ();
    [%expect {| (23) |}];
    let%bind () = Current_buffer.change_major_mode Major_mode.Prog.major_mode in
    show_in_current_buffer ();
    [%expect {| () |}];
    return ())
;;

let%expect_test "permanent buffer-locals not cleared on changing major modes" =
  Current_buffer.set_temporarily_to_temp_buffer Async (fun () ->
    Buffer_local.set_permanent int true;
    Current_buffer.set_buffer_local int (Some 23);
    show_in_current_buffer ();
    [%expect {| (23) |}];
    let%bind () = Current_buffer.change_major_mode Major_mode.Prog.major_mode in
    show_in_current_buffer ();
    [%expect {| (23) |}];
    Buffer_local.set_permanent int false;
    let%bind () = Current_buffer.change_major_mode Major_mode.Prog.major_mode in
    show_in_current_buffer ();
    [%expect {| () |}];
    return ())
;;
