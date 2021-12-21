open! Core
open! Import

let redisplay = Funcall.Wrap.("redisplay" <: bool @-> return nil)
let redisplay ?(force = false) () = redisplay force
let inhibit_redisplay = Var.Wrap.("inhibit-redisplay" <: bool)

let display_monitor_attributes_list =
  Funcall.Wrap.(
    "display-monitor-attributes-list"
    <: nullary @-> return (list (list (tuple Symbol.t value))))
;;

let monitor_attributes () = display_monitor_attributes_list ()
