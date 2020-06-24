open! Core_kernel
open! Import

let redisplay = Funcall.Wrap.("redisplay" <: bool @-> return nil)
let redisplay ?(force = false) () = redisplay force
let inhibit_redisplay = Var.Wrap.("inhibit-redisplay" <: bool)
