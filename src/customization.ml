open! Core_kernel
open! Import
include Customization_intf

module Q = struct
  include Q

  let alist = "alist" |> Symbol.intern
  let boolean = "boolean" |> Symbol.intern
  let character = "character" |> Symbol.intern
  let choice = "choice" |> Symbol.intern
  let coding_system = "coding-system" |> Symbol.intern
  let color = "color" |> Symbol.intern
  let cons = "cons" |> Symbol.intern
  let const = "const" |> Symbol.intern
  let defcustom = "defcustom" |> Symbol.intern
  let defgroup = "defgroup" |> Symbol.intern
  let directory = "directory" |> Symbol.intern
  let file = "file" |> Symbol.intern
  let float = "float" |> Symbol.intern
  let function_ = "function" |> Symbol.intern
  let group = "group" |> Symbol.intern
  let hook = "hook" |> Symbol.intern
  let integer = "integer" |> Symbol.intern
  let plist = "plist" |> Symbol.intern
  let radio = "radio" |> Symbol.intern
  let repeat = "repeat" |> Symbol.intern
  let set = "set" |> Symbol.intern
  let string = "string" |> Symbol.intern
  let variable = "variable" |> Symbol.intern
end

let customize_group = Funcall.Wrap.("customize-group" <: Symbol.t @-> return nil)
let customize_variable = Funcall.Wrap.("customize-variable" <: Symbol.t @-> return nil)
let q value = Value.list [ Symbol.to_value Q.quote; value ]

module Group = struct
  include (
    Symbol :
    sig
      type t = Symbol.t [@@deriving sexp_of]

      include Valueable.S with type t := t
    end)

  let all_defgroups = ref []

  let defgroup group_name here ~docstring ~parents =
    let group_name = group_name |> Symbol.intern in
    let form_of_parent parent =
      Form.[ Q.K.group |> symbol; parent |> Symbol.to_value |> quote ]
    in
    let docstring =
      sprintf
        "%s\n\nDefined at %s"
        (docstring |> String.strip)
        (here |> Source_code_position.to_string)
    in
    Form.(
      Blocking.eval_i
        (list
           (List.concat
              [ [ Q.defgroup |> symbol; group_name |> symbol; nil; docstring |> string ]
              ; List.concat_map parents ~f:form_of_parent
              ])));
    all_defgroups := group_name :: !all_defgroups;
    group_name
  ;;

  let of_string = Symbol.intern
  let to_string = Symbol.name
  let to_symbol t = t
  let emacs = of_string "emacs"

  let ecaml =
    defgroup "ecaml" [%here] ~docstring:{| Customization of Ecaml |} ~parents:[ emacs ]
  ;;
end

module Type = struct
  type t =
    | Alist of t * t
    | Boolean
    | Character
    | Choice of t list
    | Coding_system
    | Color
    | Cons of t * t
    | Const of Value.t
    | Directory
    | Existing_file
    | Face
    | File
    | Float
    | Function
    | Group of t
    | Hook
    | Integer
    | List of t list
    | Number
    | Option of string * t
    | Plist
    | Radio of t list
    | Regexp
    | Repeat of t
    | Set of t list
    | Sexp
    | String
    | Symbol
    | Tagged_string of string
    | Variable
    | Vector of t list
  [@@deriving sexp_of]

  let s = Symbol.to_value

  let rec vs ts = List.map ts ~f:v
  and composite s ts = Value.list (Symbol.to_value s :: vs ts)

  and v = function
    | Alist (t1, t2) ->
      Value.list [ s Q.alist; s Q.K.key_type; v t1; s Q.K.value_type; v t2 ]
    | Boolean -> s Q.boolean
    | Character -> s Q.character
    | Choice ts -> composite Q.choice ts
    | Coding_system -> s Q.coding_system
    | Color -> s Q.color
    | Cons (t1, t2) -> composite Q.cons [ t1; t2 ]
    | Const v -> Value.list [ s Q.const; v ]
    | Directory -> s Q.directory
    | Existing_file -> Value.list [ s Q.file; s Q.K.must_match; Value.t ]
    | Face -> s Q.face
    | File -> s Q.file
    | Float -> s Q.float
    | Function -> s Q.function_
    | Group t -> composite Q.group [ t ]
    | Hook -> s Q.hook
    | Integer -> s Q.integer
    | List ts -> composite Q.list ts
    | Number -> s Q.number
    | Option (none, t) ->
      Value.list
        [ s Q.choice
        ; Value.list [ s Q.const; s Q.K.tag; Value.of_utf8_bytes none; s Q.nil ]
        ; v t
        ]
    | Plist -> s Q.plist
    | Radio ts -> composite Q.radio ts
    | Regexp -> s Q.regexp
    | Repeat t -> composite Q.repeat [ t ]
    | Set ts -> composite Q.set ts
    | Sexp -> s Q.sexp
    | String -> s Q.string
    | Symbol -> s Q.symbol
    | Tagged_string tag -> Value.list [ v String; s Q.K.tag; Value.of_utf8_bytes tag ]
    | Variable -> s Q.variable
    | Vector ts -> composite Q.vector ts
  ;;

  let to_value = v
  let enum all value_of_a = Choice (List.map all ~f:(fun a -> Const (value_of_a a)))
end

let all_defcustom_symbols = ref []

module Private = struct
  let all_defcustom_symbols () =
    !all_defcustom_symbols |> List.sort ~compare:Symbol.compare_name
  ;;

  let all_defgroups () = !Group.all_defgroups |> List.sort ~compare:Symbol.compare_name
end

type 'a t = 'a Var.t [@@deriving sexp_of]

let var t = t
let symbol = Var.symbol
let value = Current_buffer0.value_exn
let custom_set_variables = Funcall.Wrap.("custom-set-variables" <: value @-> return nil)

let set_value t a =
  custom_set_variables
    (Value.list [ Var.symbol t |> Symbol.to_value; a |> Value.Type.to_value t.type_ ])
;;

let set_value_temporarily t a ~f =
  let old = value t in
  set_value t a;
  protect ~f ~finally:(fun () -> set_value t old)
;;

let standard_value = Var.default_value_exn

module Wrap = Var.Wrap

let defcustom
      ?(show_form = false)
      symbol
      here
      ~docstring
      ~group
      ~type_
      ~customization_type
      ~standard_value
      ?on_set
      ()
  =
  let standard_value = standard_value |> Value.Type.to_value type_ in
  (try
     let docstring =
       concat
         [ docstring |> String.strip
         ; "\n\n"
         ; concat [ "Customization group: "; group |> Group.to_string; "\n" ]
         ; concat [ "Standard value: "; standard_value |> Value.prin1_to_string; "\n" ]
         ; concat
             [ "Customization type:"
             ; (let string =
                  customization_type |> Type.to_value |> Value.prin1_to_string
                in
                if String.contains string '\n'
                then concat [ "\n"; string ]
                else concat [ " "; string ])
             ]
         ]
     in
     all_defcustom_symbols := symbol :: !all_defcustom_symbols;
     Load_history.add_entry here (Var symbol);
     let form =
       List.concat
         [ [ Q.defcustom |> Symbol.to_value ]
         ; [ symbol |> Symbol.to_value ]
         ; [ standard_value |> q ]
         ; [ docstring |> Value.of_utf8_bytes ]
         ; [ Q.K.group |> Symbol.to_value; group |> Symbol.to_value |> q ]
         ; [ Q.K.type_ |> Symbol.to_value; customization_type |> Type.to_value |> q ]
         ; (match on_set with
            | None -> []
            | Some on_set ->
              [ Q.K.set |> Symbol.to_value
              ; Function.to_value
                  (Defun.lambda
                     [%here]
                     (Returns Value.Type.unit)
                     (let var = Var.create symbol type_ in
                      let%map_open.Defun () = return ()
                      and _ = required "symbol" Symbol.t
                      and a = required "value" type_ in
                      on_set a;
                      (* We set the Elisp variable after calling the user-supplied
                         [on_set] function, because we only want to do the set if that
                         succeeds. *)
                      Var.set_default_value var a))
              ])
         ]
       |> Value.list
       |> Form.of_value_exn
     in
     if show_form then message_s [%sexp (form : Form.t)];
     ignore (Form.Blocking.eval form : Value.t)
   with
   | exn ->
     raise_s
       [%message
         "[defcustom] failed"
           (exn : exn)
           (symbol : Symbol.t)
           (customization_type : Type.t)
           (group : Symbol.t)
           (standard_value : Value.t)
           (docstring : string)
           ~_:(here : Source_code_position.t)]);
  Var.create symbol type_
;;

let defcustom_enum
      (type t)
      symbol
      here
      (module T : Enum_arg with type t = t)
      ~docstring
      ~group
      ~standard_value
      ?on_set
      ()
  =
  let type_ =
    Value.Type.enum
      [%sexp (Symbol.name symbol : string)]
      (module T)
      (T.to_symbol >> Symbol.to_value)
  in
  let docstring =
    concat
      ~sep:"\n"
      ((docstring |> String.strip)
       :: ""
       :: List.map T.all ~f:(fun t ->
         let docstring =
           match T.docstring t with
           | "" -> []
           | docstring -> [ ": "; docstring ]
         in
         concat ("  - " :: (t |> T.to_symbol |> Symbol.name) :: docstring)))
  in
  defcustom
    symbol
    here
    ~docstring
    ~group
    ~type_
    ~customization_type:(Type.enum T.all (Value.Type.to_value type_))
    ~standard_value
    ?on_set
    ()
;;
