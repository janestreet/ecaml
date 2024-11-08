open! Core
open! Import

type t =
  | Of_current_buffer
  | Root
  | This_abspath of File_path.Absolute.t

let to_filename = function
  | Of_current_buffer -> Current_buffer.(get_buffer_local_exn directory)
  | Root -> "/"
  | This_abspath path -> File_path.Absolute.to_string path ^ "/"
;;

let sexp_of_t = function
  | Of_current_buffer -> [%sexp "Of_current_buffer"]
  | Root -> [%sexp "Root"]
  | This_abspath path ->
    Sexp.List [ Sexp.Atom "This"; Sexp.Atom (File_path.Absolute.to_string path ^ "/") ]
;;

let within t sync_or_async ~f =
  Current_buffer.set_value_temporarily
    sync_or_async
    (Buffer_local.var Current_buffer.directory)
    (Some (t |> to_filename))
    ~f
;;
