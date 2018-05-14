open! Core_kernel
open! Import0

module Q = struct
  include Q
  let directory_file_name      = "directory-file-name"      |> Symbol.intern
  let expand_file_name         = "expand-file-name"         |> Symbol.intern
  let file_name_absolute_p     = "file-name-absolute-p"     |> Symbol.intern
  let file_name_as_directory   = "file-name-as-directory"   |> Symbol.intern
  let file_name_directory      = "file-name-directory"      |> Symbol.intern
  let file_name_extension      = "file-name-extension"      |> Symbol.intern
  let file_name_nondirectory   = "file-name-nondirectory"   |> Symbol.intern
  let file_name_sans_extension = "file-name-sans-extension" |> Symbol.intern
  let file_relative_name       = "file-relative-name"       |> Symbol.intern
end

include (String : sig
           type t = string [@@deriving sexp_of]
           include Comparable.S
             with type t := t
             with type comparator_witness = String.comparator_witness
         end)

let { Value.Type.name = _; of_value_exn; to_value } as type_ =
  { Value.Type.string with name = [%message "filename"] }

let is_absolute t =
  Symbol.funcall1 Q.file_name_absolute_p (t |> to_value) |> Value.to_bool
;;

let unary q t = Symbol.funcall1 q (t |> to_value) |> of_value_exn

let extension_exn  = unary Q.file_name_extension
let nondirectory   = unary Q.file_name_nondirectory
let of_directory   = unary Q.directory_file_name
let sans_extension = unary Q.file_name_sans_extension
let to_directory   = unary Q.file_name_as_directory

let directory t =
  let result = Symbol.funcall1 Q.file_name_directory (t |> to_value) in
  if Value.is_nil result
  then None
  else Some (result |> of_value_exn)
;;

let directory_exn t =
  match directory t with
  | Some t -> t
  | None ->
    raise_s [%message
      "[Filename.directory_exn] of filename that has no directory"
        ~filename:(t : t)]
;;

let make_relative t ~relative_to =
  Symbol.funcall2 Q.file_relative_name (t |> to_value) (relative_to |> to_value)
  |> of_value_exn
;;

let expand t ~in_dir =
  Symbol.funcall2 Q.expand_file_name (t |> to_value) (in_dir |> to_value)
  |> of_value_exn
;;
