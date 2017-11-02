open! Core_kernel
open! Import

let to_value = Value.of_utf8_bytes

let predicate q file = Symbol.funcall1 q (file |> to_value) |> Value.to_bool

let exists        = predicate Q.file_exists_p
let is_directory  = predicate Q.file_directory_p
let is_executable = predicate Q.file_executable_p
let is_readable   = predicate Q.file_readable_p
let is_regular    = predicate Q.file_regular_p
let is_symlink    = predicate Q.file_symlink_p
let is_writable   = predicate Q.file_writable_p

let is_below file ~dir =
  Symbol.funcall2 Q.file_in_directory_p
    (file |> to_value)
    (dir |> to_value)
  |> Value.to_bool
;;

let truename file =
  Symbol.funcall1 Q.file_truename (file |> to_value)
  |> Value.to_utf8_bytes_exn
;;

let delete file = Symbol.funcall1_i Q.delete_file (file |> to_value)

let copy ~src ~dst = Symbol.funcall2_i Q.copy_file (src |> to_value) (dst |> to_value)

let rename ~src ~dst =
  Symbol.funcall2_i Q.rename_file (src |> to_value) (dst |> to_value)
;;

let locate_dominating_file ~above ~basename =
  let result =
    Symbol.funcall2 Q.locate_dominating_file
      (above |> Value.of_utf8_bytes)
      (basename |> Value.of_utf8_bytes) in
  if Value.is_nil result
  then None
  else Some (result |> Filename.of_value_exn)
;;

let locate_dominating_file_exn ~above ~basename =
  match locate_dominating_file ~above ~basename with
  | Some x -> x
  | None ->
    raise_s [%message
      ""
        ~_:(concat ["Unable to find [";basename;"] in directory above [";above;"]."]
            : string)]
;;

let write ?(append = false) filename data =
  Symbol.funcall5_i Q.write_region
    (data |> Value.of_utf8_bytes)
    Value.nil
    (filename |> to_value)
    (append |> Value.of_bool)
    (Q.no_message |> Symbol.to_value) (* squelch the [Wrote file] message *)
;;
