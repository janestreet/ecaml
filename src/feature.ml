open! Core_kernel
open! Import0
include Ecaml_value.Feature

let provide = Funcall.("provide" <: Symbol.t @-> return nil)
let is_provided = Funcall.("featurep" <: Symbol.t @-> return bool)
let features = Var.Wrap.("features" <: list Symbol.t)
let all_provided () = Current_buffer0.value_exn features
