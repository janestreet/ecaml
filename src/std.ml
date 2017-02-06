open! Core
open! Async
open! Import

module Emacs = struct
  include Emacs

  module Expression = Expression
  module Function   = Function
  module Symbol     = Symbol
  module Value      = Value

  let defun     = Function.defun
  let funcall   = Value.funcall
  let funcall_i = Value.funcall_i

  let provide =
    Async_ecaml.initialize_module;
    Import.initialize_module;
    Value.initialize_module;
    Symbol.provide
  ;;
end
