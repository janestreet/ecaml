open! Core_kernel
open! Import0

include (
  String :
  sig
    type t = string [@@deriving sexp_of]

    include
      Comparable.S
      with type t := t
      with type comparator_witness = String.comparator_witness

    include Hashable.S with type t := t
  end)

let type_ = Value.Type.(map_id string) [%message "filename"]
let t = type_
let of_value_exn = Value.Type.of_value_exn type_
let to_value = Value.Type.to_value type_
let is_absolute = Funcall.("file-name-absolute-p" <: t @-> return bool)
let extension = Funcall.("file-name-extension" <: t @-> return (nil_or string))

let extension_exn t =
  match extension t with
  | Some x -> x
  | None -> raise_s [%message "Filename.extension_exn" ~_:(t : t)]
;;

let nondirectory = Funcall.("file-name-nondirectory" <: t @-> return string)
let of_directory = Funcall.("directory-file-name" <: string @-> return string)
let sans_extension = Funcall.("file-name-sans-extension" <: t @-> return string)
let to_directory = Funcall.("file-name-as-directory" <: t @-> return string)
let directory = Funcall.("file-name-directory" <: t @-> return (nil_or string))

let directory_exn t =
  match directory t with
  | Some t -> t
  | None ->
    raise_s
      [%message
        "[Filename.directory_exn] of filename that has no directory" ~filename:(t : t)]
;;

let file_relative_name = Funcall.("file-relative-name" <: t @-> t @-> return t)
let make_relative t ~relative_to = file_relative_name t relative_to
let expand_file_name = Funcall.("expand-file-name" <: t @-> t @-> return t)
let expand t ~in_dir = expand_file_name t in_dir

let temporary_file_directory =
  let temporary_file_directory = "temporary-file-directory" in
  let funcall = Funcall.(temporary_file_directory <: nullary @-> return t) in
  let variable = Var.Wrap.(temporary_file_directory <: t) in
  fun () ->
    if Symbol.function_is_defined (Symbol.intern temporary_file_directory)
    then funcall ()
    else Current_buffer0.value_exn variable
;;
