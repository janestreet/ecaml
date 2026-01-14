open! Core
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
  let key_sequence = "key-sequence" |> Symbol.intern
  let plist = "plist" |> Symbol.intern
  let radio = "radio" |> Symbol.intern
  let repeat = "repeat" |> Symbol.intern
  let quote = "quote" |> Symbol.intern
  let set = "set" |> Symbol.intern
  let string = "string" |> Symbol.intern
  let variable = "variable" |> Symbol.intern
end

let customize_group = Funcall.Wrap.("customize-group" <: Symbol.t @-> return nil)
let customize_variable = Funcall.Wrap.("customize-variable" <: Symbol.t @-> return nil)

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
    Dump.eval_and_dump ~here (fun () ->
      let docstring = docstring |> String.strip in
      require_nonempty_docstring here ~docstring;
      let form_of_parent parent =
        Form.[ Q.K.group |> symbol; parent |> Symbol.to_value |> quote ]
      in
      Form.(
        list
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
    defgroup "ecaml" [%here] ~docstring:{| Customization of Ecaml. |} ~parents:[ emacs ]
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
    | Key_sequence
    | List of t list
    | Number
    | Option of
        { doc_for_none : string
        ; t : t
        }
    | Plist
    | Radio of t list
    | Regexp
    | Repeat of t
    | Set of t list
    | Sexp
    | String
    | Symbol
    | Tag of string * t
    | Variable
    | Vector of t list
  [@@deriving sexp_of]

  let s = Form.symbol

  let rec fs ts = List.map ts ~f
  and composite s ts = Form.list (Form.symbol s :: fs ts)

  and f = function
    | Alist (t1, t2) ->
      Form.list [ s Q.alist; s Q.K.key_type; f t1; s Q.K.value_type; f t2 ]
    | Boolean -> s Q.boolean
    | Character -> s Q.character
    | Choice ts -> composite Q.choice ts
    | Coding_system -> s Q.coding_system
    | Color -> s Q.color
    | Cons (t1, t2) -> composite Q.cons [ t1; t2 ]
    | Const v -> Form.list [ s Q.const; Form.of_value_exn v ]
    | Directory -> s Q.directory
    | Existing_file -> Form.list [ s Q.file; s Q.K.must_match; Form.of_value_exn Value.t ]
    | Face -> s Q.face
    | File -> s Q.file
    | Float -> s Q.float
    | Function -> s Q.function_
    | Group t -> composite Q.group [ t ]
    | Hook -> s Q.hook
    | Integer -> s Q.integer
    | Key_sequence -> s Q.key_sequence
    | List ts -> composite Q.list ts
    | Number -> s Q.number
    | Option { doc_for_none; t } -> f (Choice [ Tag (doc_for_none, Const Value.nil); t ])
    | Plist -> s Q.plist
    | Radio ts -> composite Q.radio ts
    | Regexp -> s Q.regexp
    | Repeat t -> composite Q.repeat [ t ]
    | Set ts -> composite Q.set ts
    | Sexp -> s Q.sexp
    | String -> s Q.string
    | Symbol -> s Q.symbol
    | Tag (tag, t) ->
      let f = f t in
      let f_value = Form.to_value f in
      if Value.is_cons f_value
      then
        (* Insert [:tag TAG] immediately after constructor, for a compound type. *)
        Value.cons
          (Value.car_exn f_value)
          (Value.cons
             (Symbol.to_value Q.K.tag)
             (Value.cons (Value.of_utf8_bytes tag) (Value.cdr_exn f_value)))
        |> Form.of_value_exn
      else Form.list [ f; s Q.K.tag; Form.string tag ]
    | Variable -> s Q.variable
    | Vector ts -> composite Q.vector ts
  ;;

  let to_form = f
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
let name t = Symbol.name (symbol t)
let value = Current_buffer0.value_exn
let custom_set_variables = Funcall.Wrap.("custom-set-variables" <: value @-> return nil)

let set_value t a =
  custom_set_variables
    (Value.list
       [ Var.symbol t |> Symbol.to_value
       ; a |> Value.Type.to_value t.type_ |> Form.quote |> Form.to_value
       ])
;;

let set_value_temporarily t a ~f =
  let old = value t in
  set_value t a;
  protect ~f ~finally:(fun () -> set_value t old)
;;

let custom__standard_value =
  Funcall.Wrap.("custom--standard-value" <: Symbol.t @-> return value)
;;

let standard_value t =
  custom__standard_value (symbol t) |> Value.Type.of_value_exn (var t : _ Var.t).type_
;;

module Wrap = Var.Wrap

let add_to_load_history symbol here =
  all_defcustom_symbols := symbol :: !all_defcustom_symbols;
  Load_history.add_entry here (Var symbol)
;;

let defvaralias =
  Funcall.Wrap.("defvaralias" <: Symbol.t @-> Symbol.t @-> nil_or string @-> return nil)
;;

let defvaralias symbol here ?docstring ~alias_of () =
  defvaralias symbol alias_of docstring;
  add_to_load_history symbol here
;;

let define_obsolete_alias obsolete here ?docstring ~alias_of ~since () =
  defvaralias obsolete here ?docstring ~alias_of ();
  Obsolete.make_variable_obsolete obsolete ~current:(Some alias_of) ~since
;;

let set_default_toplevel_value =
  Funcall.Wrap.("set-default-toplevel-value" <: Symbol.t @-> value @-> return ignored)
;;

let defcustom
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
  let symbol =
    match Symbol.Automatic_migration.migrate ~old:symbol with
    | None -> symbol
    | Some { new_; since } ->
      define_obsolete_alias symbol here ~alias_of:new_ ~since ();
      new_
  in
  let standard_value = standard_value |> Value.Type.to_value type_ in
  (try
     let docstring = docstring |> String.strip in
     require_nonempty_docstring here ~docstring;
     add_to_load_history symbol here;
     let on_set =
       Option.map on_set ~f:(fun on_set ->
         Defun.defun_func
           (Symbol.intern (Symbol.name symbol ^ "--setter"))
           here
           ~docstring:"The :set function for a defcustom."
           (Returns Value.Type.unit)
           (let%map_open.Defun symbol = required "symbol" Symbol.t
            and value = required "value" value in
            on_set (Value.Type.of_value_exn type_ value);
            (* We set the Elisp variable after calling the user-supplied [on_set]
               function, because we only want to do the set if that succeeds. *)
            set_default_toplevel_value symbol value))
     in
     Dump.eval_and_dump ~here (fun () ->
       Form.apply
         Q.defcustom
         ([ Form.symbol symbol
          ; Form.quote standard_value
          ; Form.string docstring
          ; Form.symbol Q.K.group
          ; Form.quoted_symbol group
          ; Form.symbol Q.K.type_
          ; Form.list [ Form.symbol Q.quote; Type.to_form customization_type ]
          ]
          @
          match on_set with
          | None -> []
          | Some on_set -> [ Form.symbol Q.K.set; Form.quote (Function.to_value on_set) ]
         ))
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

let defcustom_enum_internal
  symbol
  here
  type_
  all
  t_docstring
  ~docstring
  ~group
  ~standard_value
  ?on_set
  ()
  =
  let docstring = docstring |> String.strip in
  require_nonempty_docstring here ~docstring;
  let docstring =
    concat
      ~sep:"\n"
      (docstring
       :: ""
       :: List.map all ~f:(fun t ->
         let docstring =
           match t_docstring t with
           | None -> []
           | Some docstring -> [ ": "; docstring ]
         in
         concat
           ("  - "
            :: (t |> Value.Type.to_value type_ |> Value.prin1_to_string)
            :: docstring)))
  in
  defcustom
    symbol
    here
    ~docstring
    ~group
    ~type_
    ~customization_type:
      (Choice
         (List.map all ~f:(fun t ->
            let const = Type.Const (Value.Type.to_value type_ t) in
            match t_docstring t with
            | None -> const
            | Some tag -> Type.Tag (tag, const))))
    ~standard_value
    ?on_set
    ()
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
  defcustom_enum_internal
    symbol
    here
    (Value.Type.enum_symbol [%sexp (Symbol.name symbol : string)] (module T))
    T.all
    T.docstring
    ~docstring
    ~group
    ~standard_value
    ?on_set
    ()
;;

let defcustom_enum_with_to_symbol
  (type t)
  symbol
  here
  (module T : Enum_arg_to_symbol with type t = t)
  ~docstring
  ~group
  ~standard_value
  ?on_set
  ()
  =
  defcustom_enum_internal
    symbol
    here
    (Value.Type.enum
       [%sexp (Symbol.name symbol : string)]
       (module T)
       (T.to_symbol >> Symbol.to_value))
    T.all
    T.docstring
    ~docstring
    ~group
    ~standard_value
    ?on_set
    ()
;;
