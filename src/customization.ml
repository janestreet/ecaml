open! Core_kernel
open! Import
include Customization_intf

module Q = struct
  include Q

  let alist = "alist" |> Symbol.intern
  and boolean = "boolean" |> Symbol.intern
  and character = "character" |> Symbol.intern
  and choice = "choice" |> Symbol.intern
  and coding_system = "coding-system" |> Symbol.intern
  and color = "color" |> Symbol.intern
  and cons = "cons" |> Symbol.intern
  and const = "const" |> Symbol.intern
  and customize_group = "customize-group" |> Symbol.intern
  and customize_variable = "customize-variable" |> Symbol.intern
  and defcustom = "defcustom" |> Symbol.intern
  and defgroup = "defgroup" |> Symbol.intern
  and directory = "directory" |> Symbol.intern
  and float = "float" |> Symbol.intern
  and function_ = "function" |> Symbol.intern
  and group = "group" |> Symbol.intern
  and hook = "hook" |> Symbol.intern
  and integer = "integer" |> Symbol.intern
  and plist = "plist" |> Symbol.intern
  and radio = "radio" |> Symbol.intern
  and repeat = "repeat" |> Symbol.intern
  and string = "string" |> Symbol.intern
  and variable = "variable" |> Symbol.intern
end

module F = struct
  open Funcall

  let customize_group = Q.customize_group <: Symbol.type_ @-> return nil
  let customize_variable = Q.customize_variable <: Symbol.type_ @-> return nil
end

let customize_variable = F.customize_variable
let customize_group = F.customize_group
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
    let form_of_parent parent =
      Form.[ Q.K.group |> symbol; parent |> Symbol.to_value |> quote ]
    in
    let docstring =
      sprintf "%s\n\nDefined at %s" docstring (here |> Source_code_position.to_string)
    in
    Form.(
      eval_i
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

let defcustom
      ?(show_form = false)
      symbol
      here
      ~docstring
      ~group
      ~type_
      ~customization_type
      ~standard_value
      ()
  =
  let standard_value = standard_value |> Value.Type.to_value type_ in
  (try
     let docstring =
       concat
         [ docstring |> String.strip
         ; "\n\n"
         ; concat [ "Customization group: "; group |> Group.to_string; "\n" ]
         ; concat
             [ "Customization type:"
             ; (let string =
                  [%sexp (customization_type : Type.t)] |> Sexp.to_string_hum
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
       [ Q.defcustom |> Symbol.to_value
       ; symbol |> Symbol.to_value
       ; standard_value |> q
       ; docstring |> Value.of_utf8_bytes
       ; Q.K.group |> Symbol.to_value
       ; group |> Symbol.to_value |> q
       ; Q.K.type_ |> Symbol.to_value
       ; customization_type |> Type.to_value |> q
       ]
       |> Value.list
       |> Form.of_value_exn
     in
     if show_form then Echo_area.message_s [%sexp (form : Form.t)];
     ignore (Form.eval form : Value.t)
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

module Enum = struct
  module type Arg = Enum_arg
  module type S = Enum

  let make (type t) symbol here (module T : Arg with type t = t) ~docstring ~group =
    ( module struct
      type t = T.t

      let ({ Value.Type.of_value_exn; to_value; id = _ } as type_) =
        Value.Type.enum
          [%sexp (Symbol.name symbol : string)]
          (module T)
          (T.to_symbol >> Symbol.to_value)
      ;;

      let var = Var.create symbol type_

      let docstring =
        concat
          ~sep:"\n"
          (docstring
           :: ""
           :: List.map T.all ~f:(fun t ->
             let docstring =
               match T.docstring t with
               | "" -> []
               | docstring -> [ ": "; docstring ]
             in
             concat ("  - " :: (t |> T.to_symbol |> Symbol.name) :: docstring)))
      ;;

      let initialize_defcustom () =
        ignore
          ( defcustom
              symbol
              here
              ~docstring
              ~group
              ~type_
              ~customization_type:(Type.enum T.all to_value)
              ~standard_value:T.standard_value
              ()
            : _ Var.t )
      ;;
    end
    : S
      with type t = t )
  ;;
end
