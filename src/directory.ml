open! Core_kernel
open! Import

module Q = struct
  include Q
  let delete_directory                 = "delete-directory"                 |> Symbol.intern
  let directory_files                  = "directory-files"                  |> Symbol.intern
  let directory_files_recursively      = "directory-files-recursively"      |> Symbol.intern
  let make_directory                   = "make-directory"                   |> Symbol.intern
end

let create ?(parents = false) dirname =
  Symbol.funcall2_i Q.make_directory
    (dirname |> Filename.to_value)
    (parents |> Value.of_bool)
;;

let delete ?(recursive = false) dirname =
  Symbol.funcall2_i Q.delete_directory
    (dirname |> Filename.to_value)
    (recursive |> Value.of_bool)
;;

let files
      ?(absolute = false)
      ?(include_dot_and_dotdot = false)
      ?matching
      ?(sort = true)
      dirname =
  let files =
    Symbol.funcall4 Q.directory_files
      (dirname |> Filename.to_value)
      (absolute |> Value.of_bool)
      (matching |> (Value.Type.option Regexp.type_).to_value)
      (sort |> not |> Value.of_bool)
    |> (Value.Type.list Filename.type_).of_value_exn in
  if include_dot_and_dotdot
  then files
  else (
    List.filter files ~f:(fun file ->
      match Filename.nondirectory file with
      | "." | ".." -> false
      | _ -> true))
;;

let files_recursively
      ?(include_directories = false)
      ?(matching = Regexp.match_anything)
      dirname =
  Symbol.funcall3 Q.directory_files_recursively
    (dirname |> Filename.to_value)
    (matching |> Regexp.to_value)
    (include_directories |> Value.of_bool)
  |> (Value.Type.list Filename.type_).of_value_exn
;;
