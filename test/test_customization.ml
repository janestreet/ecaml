open! Core_kernel
open! Import
open! Customization

let counter = ref 0

let next_counter () =
  incr counter;
  !counter
;;

let test ?show_form type_ customization_type standard_value =
  let group = Group.of_string "test-customization-group" in
  let variable =
    concat [ "test-customization-symbol-"; next_counter () |> Int.to_string ]
    |> Symbol.intern
  in
  ignore
    ( defcustom
        variable
        [%here]
        ~docstring:""
        ~group
        ~type_
        ~customization_type
        ~standard_value
        ?show_form
        ()
      : _ Var.t );
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
     \nCustomization type: Boolean" :group (quote test-customization-group) :type
     (quote boolean))

    Hide Test Customization Symbol 1: [Toggle]  off (nil)
       [ State ]: STANDARD.
        Hide

       Customization group: test-customization-group
       Customization type: Boolean
    Groups: [Test Customization Group] |}]
;;

let%expect_test "[Boolean] with default [t]" =
  test Value.Type.bool Boolean true;
  [%expect
    {|
    Hide Test Customization Symbol 2: [Toggle]  on (non-nil)
       [ State ]: STANDARD.
        Hide

       Customization group: test-customization-group
       Customization type: Boolean
    Groups: [Test Customization Group] |}]
;;

let%expect_test "[Const]" =
  (* The customize UI doesn't know how to handle [Const _] outside [Choice].  *)
  test Value.Type.int (Choice [ Const (Value.Type.int.to_value 13) ]) 13;
  [%expect
    {|
    Hide Test Customization Symbol 3: [Value Menu] 13
       [ State ]: STANDARD.
        Hide

       Customization group: test-customization-group
       Customization type: (Choice ((Const 13)))
    Groups: [Test Customization Group] |}]
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
  end
  in
  test M.type_ (Type.enum M.all M.to_value) A;
  [%expect
    {|
    Hide Test Customization Symbol 4: [Value Menu] A
       [ State ]: STANDARD.
        Hide

       Customization group: test-customization-group
       Customization type: (Choice ((Const A) (Const B)))
    Groups: [Test Customization Group] |}]
;;

let%expect_test "[defcustom] invalid-value error message" =
  let var =
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
  Current_buffer.set_value { var with type_ = Value.Type.bool } false;
  show_raise (fun () -> Current_buffer.value_exn var);
  [%expect
    {|
    (raised (
      "invalid value for variable: zzz" (
        "unable to convert Elisp value to OCaml value"
        (type_ int)
        (value nil)
        (exn (wrong-type-argument (integerp nil)))))) |}]
;;
