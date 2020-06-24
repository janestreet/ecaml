open! Core_kernel
open! Import

let make_obsolete =
  Funcall.Wrap.(
    "make-obsolete" <: Symbol.t @-> nil_or Symbol.t @-> string @-> return nil)
;;

let make_function_obsolete obsolete ~current ~since =
  make_obsolete obsolete current since
;;

let make_obsolete_variable =
  Funcall.Wrap.(
    "make-obsolete-variable" <: Symbol.t @-> nil_or Symbol.t @-> string @-> return nil)
;;

let make_variable_obsolete obsolete ~current ~since =
  make_obsolete_variable obsolete current since
;;
