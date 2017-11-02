open! Core_kernel
open! Import

type t =
  | Of_current_buffer
  | Root
  | This of string
[@@deriving sexp_of]

let to_filename = function
  | Of_current_buffer -> Current_buffer.(value_exn directory)
  | Root -> "/"
  | This s -> s
;;

let within t ~f =
  Current_buffer.set_value_temporarily Current_buffer.directory (t |> to_filename) ~f
;;
