open! Core
open! Async_kernel
open! Import
open! Customization

let group = Group.of_string "test-customization-group"

let test type_ customization_type standard_value =
  let variable = Symbol.create_uninterned ~name:"test-customization-symbol" in
  ignore
    (defcustom
       variable
       [%here]
       ~docstring:"<docstring>"
       ~group
       ~type_
       ~customization_type
       ~standard_value
       ()
     : _ Customization.t);
  ignoring_stderr (fun () -> customize_variable variable);
  Current_buffer.contents ()
  |> Text.to_utf8_bytes
  |> String.split_lines
  |> (fun l -> List.drop l 7)
  |> String.concat ~sep:"\n"
  |> print_endline
;;

let%expect_test "[Boolean] with default [nil]" =
  test Value.Type.bool Boolean false;
  [%expect
    {|
    Hide Test Customization Symbol: Boolean: [Toggle]  off (nil)
       [ State ]: STANDARD.
       <docstring>
    Groups: [Test Customization Group]
    |}];
  return ()
;;

let%expect_test "[Boolean] with default [t]" =
  test Value.Type.bool Boolean true;
  [%expect
    {|
    Hide Test Customization Symbol: Boolean: [Toggle]  on (non-nil)
       [ State ]: STANDARD.
       <docstring>
    Groups: [Test Customization Group]
    |}];
  return ()
;;

let%expect_test "[Const]" =
  (* The customize UI doesn't know how to handle [Const _] outside [Choice]. *)
  test Value.Type.int (Choice [ Const (Value.Type.(to_value int) 13) ]) 13;
  [%expect
    {|
    Hide Test Customization Symbol: Choice: [Value Menu] 13
       [ State ]: STANDARD.
       <docstring>
    Groups: [Test Customization Group]
    |}];
  return ()
;;

let%expect_test "[Tag]" =
  test
    Value.Type.int
    (Choice
       [ Tag ("the loneliest number", Const (Value.Type.(to_value int) 1))
         (* compound customization type *)
       ; Tag ("the second-loneliest number", Const (Value.Type.(to_value int) 2))
       ; Tag ("some other number", Integer) (* atomic customization type *)
       ])
    2;
  [%expect
    {|
    Hide Test Customization Symbol: Choice: [Value Menu] the second-loneliest number
       [ State ]: STANDARD.
       <docstring>
    Groups: [Test Customization Group]
    |}];
  return ()
;;

let%expect_test "nested [Tag]" =
  (* Emacs just picks one of the tags when there are multiple; in this case, the outermost
     one. *)
  test Value.Type.bool (Choice [ Tag ("a", Tag ("b", Tag ("c", Const Value.nil))) ]) false;
  [%expect
    {|
    Hide Test Customization Symbol: Choice: [Value Menu] a
       [ State ]: STANDARD.
       <docstring>
    Groups: [Test Customization Group]
    |}];
  return ()
;;

let%expect_test "[enum]" =
  let module M = struct
    module T = struct
      type t =
        | A
        | B
      [@@deriving enumerate, sexp]
    end

    include T
    include Sexpable.To_stringable (T)

    let of_value_exn _ = raise_s [%message "unimplemented"]
    let to_value t = Symbol.to_value (Symbol.intern (to_string t))
    let type_ = Value.Type.create [%message "M"] [%sexp_of: t] of_value_exn to_value
    let t = type_
  end
  in
  test M.t ((Type.enum [@alert "-deprecated"]) M.all M.to_value) A;
  [%expect
    {|
    Hide Test Customization Symbol: Choice: [Value Menu] A
       [ State ]: STANDARD.
       <docstring>
    Groups: [Test Customization Group]
    |}];
  return ()
;;

let%expect_test "[defcustom] invalid-value error message" =
  let customization =
    defcustom
      ("zzz" |> Symbol.intern)
      [%here]
      ~docstring:"<docstring>"
      ~group
      ~type_:Value.Type.int
      ~customization_type:Integer
      ~standard_value:13
      ()
  in
  Current_buffer.set_value
    { (Customization.var customization) with type_ = Value.Type.bool }
    false;
  show_raise (fun () -> Customization.value customization);
  [%expect
    {|
    (raised (
      "invalid value for variable: zzz" (
        "unable to convert Elisp value to OCaml value"
        (type_ int)
        (value nil)
        (exn (wrong-type-argument (integerp nil))))))
    |}];
  return ()
;;

let%expect_test "[defcustom ~on_set]" =
  let t =
    defcustom
      ("foo" |> Symbol.intern)
      [%here]
      ~docstring:"<docstring>"
      ~group
      ~type_:Value.Type.int
      ~customization_type:Integer
      ~standard_value:13
      ~on_set:(fun i ->
        if i < 0 then raise_s [%message "can't set it to a negative"];
        message_s [%message "set it"])
      ()
  in
  [%expect {| set it |}];
  let show () = print_s [%sexp (value t : int)] in
  show ();
  [%expect {| 13 |}];
  set_value t 15;
  [%expect {| set it |}];
  show ();
  [%expect {| 15 |}];
  set_value t (-1);
  [%expect {| Error setting foo: (error can't set it to a negative) |}];
  show ();
  [%expect {| 15 |}];
  return ()
;;

let%expect_test "[set_value] for compound values" =
  let t =
    defcustom
      ("list-of-ints" |> Symbol.intern)
      [%here]
      ~docstring:"<docstring>"
      ~group
      ~type_:Value.Type.(list int)
      ~standard_value:[ 1; 2; 3 ]
      ~customization_type:(Repeat Integer)
      ~on_set:(fun value -> print_s [%message "Setting var" (value : int list)])
      ()
  in
  [%expect {| ("Setting var" (value (1 2 3))) |}];
  set_value t [ 4; 5; 6 ];
  [%expect {| ("Setting var" (value (4 5 6))) |}];
  print_s (Customization.value t |> [%sexp_of: int list]);
  [%expect {| (4 5 6) |}];
  return ()
;;

let%expect_test "[standard_value]" =
  let t =
    defcustom
      ("my-favorite-symbol" |> Symbol.intern)
      [%here]
      ~docstring:"<docstring>"
      ~group
      ~type_:Symbol.t
      ~standard_value:("aaaaa" |> Symbol.intern)
      ~customization_type:Symbol
      ()
  in
  let show () =
    print_s
      [%sexp
        { value = (Customization.value t : Symbol.t)
        ; standard_value = (Customization.standard_value t : Symbol.t)
        }]
  in
  show ();
  [%expect
    {|
    ((value          aaaaa)
     (standard_value aaaaa))
    |}];
  Customization.set_value t ("bbbbb" |> Symbol.intern);
  show ();
  [%expect
    {|
    ((value          bbbbb)
     (standard_value aaaaa))
    |}];
  return ()
;;
