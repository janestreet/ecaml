open! Core_kernel
open! Import
open! Var

let%expect_test "[default_value_exn] raise" =
  show_raise (fun () -> default_value_exn (int_var "z"));
  [%expect {|
    (raised (void-variable (z))) |}];
;;

let%expect_test "[default_value_exn]" =
  let t = int_var "z" in
  Current_buffer.set_value t 13;
  print_s [%sexp (default_value_exn t : int)];
  [%expect {|
    13 |}];
;;

let%expect_test "[set_default_value]" =
  let t = int_var "z" in
  set_default_value t 13;
  print_s [%sexp (default_value_exn t : int)];
  [%expect {|
    13 |}];
;;

let%expect_test "[default_value_is_defined]" =
  let t = int_var "z" in
  print_s [%sexp (default_value_is_defined t : bool)];
  [%expect {|
    false |}];
  Current_buffer.set_value t 13;
  print_s [%sexp (default_value_is_defined t : bool)];
  [%expect {|
    true |}];
;;

let%expect_test "[set_default_value]" =
  let t = int_var "z" in
  print_s [%sexp (default_value_is_defined t : bool)];
  [%expect {|
    false |}];
  set_default_value t 13;
  print_s [%sexp (default_value_is_defined t : bool)];
  [%expect {|
    true |}];
;;

let%expect_test "[make_buffer_local_always]" =
  let t = int_var "z" in
  make_buffer_local_always t;
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    print_s [%sexp (Current_buffer.is_buffer_local t : bool)];
    [%expect {|
      false |}];
    Current_buffer.set_value t 13;
    print_s [%sexp (Current_buffer.is_buffer_local t : bool)];
    [%expect {|
      true |}]);
;;

let%expect_test "sexpable value in a var" =
  let module A = struct
    type t =
      { a : string
      ; b : int
      }
    [@@deriving sexp]
  end
  in
  let a_type = Value.Type.sexpable ~name:(Sexp.of_string "A") (module A) in
  let t = create (Symbol.create ~name:"a-and-marker") (Value.Type.tuple a_type Marker.type_) in
  make_buffer_local_always t;
  let setup buf ~a ~b =
    Current_buffer.set_temporarily buf ~f:(fun () ->
      Point.goto_min ();
      let marker = Point.marker_at () in
      Marker.set_insertion_type marker After_inserted_text;
      Current_buffer.set_value t ({ A.a; b}, marker);
    )
  in
  let move_marker buf ~by =
    Current_buffer.set_temporarily buf ~f:(fun () ->
      Point.goto_min ();
      Point.insert (String.init by ~f:(fun _ -> 'A')))
  in
  let read buf =
    Current_buffer.set_temporarily buf ~f:(fun () ->
      let v = Current_buffer.value_exn t in
      print_s [%message
        ""
          ~buffer:(Buffer.name buf : string option)
          ~var:(v : A.t * Marker.t) ])
  in
  let buf1 = Buffer.create ~name:"buf1" in
  let buf2 = Buffer.create ~name:"buf2" in
  setup buf1 ~a:"foo" ~b:1;
  setup buf2 ~a:"bar" ~b:2;
  move_marker buf1 ~by:10;
  move_marker buf2 ~by:5;
  read buf1;
  read buf2;
  Buffer.kill buf1; Buffer.kill buf2;
  [%expect {|
    ((buffer (buf1))
     (var (
       ((a foo)
        (b 1))
       "#<marker (moves after insertion) at 11 in buf1>")))
    ((buffer (buf2))
     (var (
       ((a bar)
        (b 2))
       "#<marker (moves after insertion) at 6 in buf2>"))) |}]
;;

let%expect_test "caml_embed value" =
  let module A = struct
    type t =
      { a : string
      ; b : int
      }
    [@@deriving sexp_of]
    let type_id = Type_equal.Id.create ~name:"A" sexp_of_t
  end
  in
  let a_type = Value.Type.caml_embed A.type_id in
  let var_name = "embedded-var-a" in
  let t = create (Symbol.create ~name:var_name) a_type in
  make_buffer_local_always t;
  let orig : A.t = { a = "Foo"; b = 100 } in
  Current_buffer.set_value t orig;
  let from_emacs = Current_buffer.value_exn t in
  print_s [%message
    ""
      (orig : A.t)
      (from_emacs : A.t)
      ~phys_equal:(phys_equal orig from_emacs : bool)
  ];
  [%expect {|
    ((orig       ((a Foo) (b 100)))
     (from_emacs ((a Foo) (b 100)))
     (phys_equal true)) |}];
;;
