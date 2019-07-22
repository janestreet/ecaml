open! Core_kernel
open! Async_kernel
open! Import
open! Customization

let counter = ref 0

let next_counter () =
  incr counter;
  !counter
;;

let group = Group.of_string "test-customization-group"

let test ?show_form type_ customization_type standard_value =
  let variable =
    concat [ "test-customization-symbol-"; next_counter () |> Int.to_string ]
    |> Symbol.intern
  in
  ignore
    (defcustom
       variable
       [%here]
       ~docstring:""
       ~group
       ~type_
       ~customization_type
       ~standard_value
       ?show_form
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
  test Value.Type.bool Boolean false ~show_form:true;
  [%expect
    {|
    (defcustom test-customization-symbol-1 (quote nil)
      "\
     \n\
     \nCustomization group: test-customization-group\
     \nStandard value: nil\
     \nCustomization type: boolean" :group (quote test-customization-group) :type
     (quote boolean))

    Hide Test Customization Symbol 1: [Toggle]  off (nil)
       [ State ]: STANDARD.
        Hide

       Customization group: test-customization-group
       Standard value: nil
       Customization type: boolean
    Groups: [Test Customization Group] |}];
  return ()
;;

let%expect_test "[Boolean] with default [t]" =
  test Value.Type.bool Boolean true;
  [%expect
    {|
    Hide Test Customization Symbol 2: [Toggle]  on (non-nil)
       [ State ]: STANDARD.
        Hide

       Customization group: test-customization-group
       Standard value: t
       Customization type: boolean
    Groups: [Test Customization Group] |}];
  return ()
;;

let%expect_test "[Const]" =
  (* The customize UI doesn't know how to handle [Const _] outside [Choice].  *)
  test Value.Type.int (Choice [ Const (Value.Type.(to_value int) 13) ]) 13;
  [%expect
    {|
    Hide Test Customization Symbol 3: [Value Menu] 13
       [ State ]: STANDARD.
        Hide

       Customization group: test-customization-group
       Standard value: 13
       Customization type: (choice (const 13))
    Groups: [Test Customization Group] |}];
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
  test M.t (Type.enum M.all M.to_value) A;
  [%expect
    {|
    Hide Test Customization Symbol 4: [Value Menu] A
       [ State ]: STANDARD.
        Hide

       Customization group: test-customization-group
       Standard value: A
       Customization type: (choice (const A) (const B))
    Groups: [Test Customization Group] |}];
  return ()
;;

let%expect_test "[defcustom] invalid-value error message" =
  let customization =
    defcustom
      ("zzz" |> Symbol.intern)
      [%here]
      ~docstring:""
      ~group:("test-customization-group" |> Group.of_string)
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
        (exn (wrong-type-argument (integerp nil)))))) |}];
  return ()
;;

let%expect_test "[defcustom ~on_set]" =
  let t =
    defcustom
      ("foo" |> Symbol.intern)
      [%here]
      ~docstring:""
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
