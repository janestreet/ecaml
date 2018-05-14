(** [Q] is short for Emacs [quote].  [Q] defines constant symbols.

    The naming convention is to use the emacs name but replace '-' with '_'. *)

open! Core_kernel
open! Import0

let add_text_properties              = "add-text-properties"              |> Symbol.intern
let append                           = "append"                           |> Symbol.intern
let aref                             = "aref"                             |> Symbol.intern
let aset                             = "aset"                             |> Symbol.intern
let buffer_file_name                 = "buffer-file-name"                 |> Symbol.intern
let buffer_local_variables           = "buffer-local-variables"           |> Symbol.intern
let buffer_read_only                 = "buffer-read-only"                 |> Symbol.intern
let cons                             = "cons"                             |> Symbol.intern
let default_value                    = "default-value"                    |> Symbol.intern
let defun                            = "defun"                            |> Symbol.intern
let defvar                           = "defvar"                           |> Symbol.intern
let face                             = "face"                             |> Symbol.intern
let file                             = "file"                             |> Symbol.intern
let funcall                          = "funcall"                          |> Symbol.intern
let kill_buffer                      = "kill-buffer"                      |> Symbol.intern
let length                           = "length"                           |> Symbol.intern
let list                             = "list"                             |> Symbol.intern
let nil                              = "nil"                              |> Symbol.intern
let number                           = "number"                           |> Symbol.intern
let point                            = "point"                            |> Symbol.intern
let provide                          = "provide"                          |> Symbol.intern
let put_text_property                = "put-text-property"                |> Symbol.intern
let quote                            = "quote"                            |> Symbol.intern
let regexp                           = "regexp"                           |> Symbol.intern
let require                          = "require"                          |> Symbol.intern
let set                              = "set"                              |> Symbol.intern
let set_marker                       = "set-marker"                       |> Symbol.intern
let set_text_properties              = "set-text-properties"              |> Symbol.intern
let sexp                             = "sexp"                             |> Symbol.intern
let symbol                           = "symbol"                           |> Symbol.intern
let symbol_value                     = "symbol-value"                     |> Symbol.intern
let t                                = "t"                                |> Symbol.intern
let vector                           = "vector"                           |> Symbol.intern

(** [A] is short for "ampersand".  [A] defines symbols starting with [&]. *)
module A = struct
  let optional = "&optional" |> Symbol.intern
  let rest     = "&rest"     |> Symbol.intern
end

(** [K] is short for "keyword".  [K] defines a symbols starting with ":". *)
module K = struct
  let around         = ":around"         |> Symbol.intern
  let background     = ":background"     |> Symbol.intern
  let box            = ":box"            |> Symbol.intern
  let family         = ":family"         |> Symbol.intern
  let file           = ":file"           |> Symbol.intern
  let filter         = ":filter"         |> Symbol.intern
  let font           = ":font"           |> Symbol.intern
  let foreground     = ":foreground"     |> Symbol.intern
  let foundry        = ":foundry"        |> Symbol.intern
  let group          = ":group"          |> Symbol.intern
  let height         = ":height"         |> Symbol.intern
  let inherit_       = ":inherit"        |> Symbol.intern
  let inverse_video  = ":inverse-video"  |> Symbol.intern
  let key_type       = ":key-type"       |> Symbol.intern
  let must_match     = ":must-match"     |> Symbol.intern
  let name           = ":name"           |> Symbol.intern
  let overline       = ":overline"       |> Symbol.intern
  let pad_right      = ":pad-right"      |> Symbol.intern
  let right_align    = ":right-align"    |> Symbol.intern
  let server         = ":server"         |> Symbol.intern
  let service        = ":service"        |> Symbol.intern
  let slant          = ":slant"          |> Symbol.intern
  let stipple        = ":stipple"        |> Symbol.intern
  let strike_through = ":strike-through" |> Symbol.intern
  let tag            = ":tag"            |> Symbol.intern
  let type_          = ":type"           |> Symbol.intern
  let underline      = ":underline"      |> Symbol.intern
  let value_type     = ":value-type"     |> Symbol.intern
  let weight         = ":weight"         |> Symbol.intern
  let width          = ":width"          |> Symbol.intern
end
