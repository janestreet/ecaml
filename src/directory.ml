open! Core_kernel
open! Import
open! Ecaml_filename

let make_directory =
  Funcall.Wrap.("make-directory" <: Filename.t @-> bool @-> return nil)
;;

let create ?(parents = false) dirname = make_directory dirname parents

let delete_directory =
  Funcall.Wrap.("delete-directory" <: Filename.t @-> bool @-> return nil)
;;

let delete ?(recursive = false) dirname = delete_directory dirname recursive

let directory_files =
  Funcall.Wrap.(
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
  Funcall.Wrap.(
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
  Funcall.Wrap.("make-temp-file" <: string @-> bool @-> string @-> return Filename.t)
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

let for_temp_files_customization =
  Customization.Wrap.("temporary-file-directory" <: Filename.t)
;;

let for_temp_files () = Customization.value for_temp_files_customization

let for_temp_files_of_current_buffer =
  let function_symbol = Symbol.intern "temporary-file-directory" in
  let funcall =
    Funcall.Wrap.("temporary-file-directory" <: nullary @-> return Filename.t)
  in
  fun () ->
    if Symbol.function_is_defined function_symbol then funcall () else for_temp_files ()
;;
