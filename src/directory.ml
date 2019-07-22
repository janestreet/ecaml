open! Core_kernel
open! Import
open! Ecaml_filename

let make_directory = Funcall.("make-directory" <: Filename.t @-> bool @-> return nil)
let create ?(parents = false) dirname = make_directory dirname parents
let delete_directory = Funcall.("delete-directory" <: Filename.t @-> bool @-> return nil)
let delete ?(recursive = false) dirname = delete_directory dirname recursive

let directory_files =
  Funcall.(
    "directory-files"
    <: Filename.t @-> bool @-> nil_or Regexp.t @-> bool @-> return (list Filename.t))
;;

let files
      ?(absolute = false)
      ?(include_dot_and_dotdot = false)
      ?matching
      ?(sort = true)
      dirname
  =
  let files = directory_files dirname absolute matching (not sort) in
  if include_dot_and_dotdot
  then files
  else
    List.filter files ~f:(fun file ->
      match Filename.nondirectory file with
      | "." | ".." -> false
      | _ -> true)
;;

let directory_files_recursively =
  Funcall.(
    "directory-files-recursively"
    <: Filename.t @-> Regexp.t @-> bool @-> return (list Filename.t))
;;

let files_recursively
      ?(include_directories = false)
      ?(matching = Regexp.match_anything)
      dirname
  =
  directory_files_recursively dirname matching include_directories
;;

let make_temp_file =
  Funcall.("make-temp-file" <: string @-> bool @-> string @-> return Filename.t)
;;

let make_temp_dir ~prefix ~suffix = make_temp_file prefix true suffix

let with_temp_dir sync_or_async ~f ~prefix ~suffix =
  let filename = make_temp_dir ~prefix ~suffix in
  Sync_or_async.protect
    [%here]
    ~allow_in_background:true
    sync_or_async
    ~f:(fun () -> f filename)
    ~finally:(fun () -> delete filename ~recursive:true)
;;
