open! Core_kernel
open! Import

module Q = struct
  include Q
  let alist                            = "alist"                            |> Symbol.intern
  let boolean                          = "boolean"                          |> Symbol.intern
  let character                        = "character"                        |> Symbol.intern
  let choice                           = "choice"                           |> Symbol.intern
  let coding_system                    = "coding-system"                    |> Symbol.intern
  let color                            = "color"                            |> Symbol.intern
  let const                            = "const"                            |> Symbol.intern
  let defcustom                        = "defcustom"                        |> Symbol.intern
  let directory                        = "directory"                        |> Symbol.intern
  let float                            = "float"                            |> Symbol.intern
  let function_                        = "function"                         |> Symbol.intern
  let group                            = "group"                            |> Symbol.intern
  let hook                             = "hook"                             |> Symbol.intern
  let integer                          = "integer"                          |> Symbol.intern
  let plist                            = "plist"                            |> Symbol.intern
  let radio                            = "radio"                            |> Symbol.intern
  let repeat                           = "repeat"                           |> Symbol.intern
  let string                           = "string"                           |> Symbol.intern
  let variable                         = "variable"                         |> Symbol.intern
end

let q value = Value.list [ Symbol.to_value Q.quote; value ]

module Group = struct
  type t = Symbol.t [@@deriving sexp_of]

  let of_string = Symbol.intern
  let to_string = Symbol.name
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
    | Alist (t1, t2) -> Value.list [ s Q.alist ;
                                     s Q.K.key_type; v t1;
                                     s Q.K.value_type; v t2;]
    | Boolean       -> s Q.boolean
    | Character     -> s Q.character
    | Choice ts     -> composite Q.choice ts
    | Coding_system -> s Q.coding_system
    | Color         -> s Q.color
    | Cons (t1, t2) -> composite Q.cons [ t1; t2 ]
    | Const v       -> Value.list [ s Q.const; q v ]
    | Directory     -> s Q.directory
    | Existing_file -> Value.list [ s Q.file; s Q.K.must_match; Value.t ]
    | Face          -> s Q.face
    | File          -> s Q.file
    | Float         -> s Q.float
    | Function      -> s Q.function_
    | Group t       -> composite Q.group [ t ]
    | Hook          -> s Q.hook
    | Integer       -> s Q.integer
    | List ts       -> composite Q.list ts
    | Number        -> s Q.number
    | Option (none, t) -> Value.list [ s Q.choice
                                     ; (Value.list [ s Q.const ; s Q.K.tag ;
                                                     Value.of_utf8_bytes none; s Q.nil ])
                                     ; v t ]
    | Plist         -> s Q.plist
    | Radio ts      -> composite Q.radio ts
    | Regexp        -> s Q.regexp
    | Repeat t      -> composite Q.repeat [ t ]
    | Set ts        -> composite Q.set ts
    | Sexp          -> s Q.sexp
    | String        -> s Q.string
    | Symbol        -> s Q.symbol
    | Tagged_string tag -> Value.list [ v String; s Q.K.tag; Value.of_utf8_bytes tag ]
    | Variable      -> s Q.variable
    | Vector ts     -> composite Q.vector ts
  ;;

  let to_value = v
end

let defcustom here option customization_type ~docstring ~group ~standard_value =
  try
    Load_history.add_entry here (Var option);
    ignore (
      Form.eval (
        [ Q.defcustom        |> Symbol.to_value
        ; option             |> Symbol.to_value
        ; standard_value     |> q
        ; docstring          |> Value.of_utf8_bytes
        ; Q.K.group          |> Symbol.to_value
        ; group              |> Symbol.to_value |> q
        ; Q.K.type_          |> Symbol.to_value
        ; customization_type |> Type.to_value |> q ]
        |> Value.list
        |> Form.of_value_exn)
      : Value.t);
  with exn ->
    raise_s [%message
      "[defcustom] failed"
        (exn                : exn)
        (option             : Symbol.t)
        (customization_type : Type.t)
        (group              : Symbol.t)
        (standard_value     : Value.t)
        (docstring          : string)
        ~_:(here            : Source_code_position.t)]
;;
