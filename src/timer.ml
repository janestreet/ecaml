open! Core
open! Import

include Value.Make_subtype (struct
    let name = "timer"
    let here = [%here]
    let is_in_subtype = Value.is_timer
  end)

let timer_list = Var.Wrap.("timer-list" <: list t)
let timer_list_as_value = Var.Wrap.("timer-list" <: value)
let all_scheduled () = Current_buffer.value_exn timer_list
let memq = Funcall.Wrap.("memq" <: t @-> value @-> return bool)
let is_scheduled t = memq t (Current_buffer.value_exn timer_list_as_value)
let to_seconds span = span |> Time_ns.Span.to_sec

let run_at_time =
  Funcall.Wrap.("run-at-time" <: float @-> nil_or float @-> Function.t @-> return t)
;;

let run_with_idle_timer =
  Funcall.Wrap.(
    "run-with-idle-timer" <: float @-> nil_or float @-> Function.t @-> return t)
;;

let run_with_timer ~idle ?repeat span func =
  let make_timer = if idle then run_with_idle_timer else run_at_time in
  make_timer (span |> to_seconds) (repeat |> Option.map ~f:to_seconds) func
;;

let run_after = run_with_timer ~idle:false
let run_after_i ?repeat span func = ignore (run_after ?repeat span func : t)
let run_after_idle = run_with_timer ~idle:true
let run_after_idle_i ?repeat span func = ignore (run_after_idle ?repeat span func : t)
let cancel = Funcall.Wrap.("cancel-timer" <: t @-> return nil)

let sit_for =
  let sit_for = Funcall.Wrap.("sit-for" <: float @-> bool @-> return nil) in
  fun ?(redisplay = true) span ->
    Value.Private.run_outside_async (fun () ->
      sit_for (span |> to_seconds) (not redisplay))
;;

let sleep_for =
  let sleep_for = Funcall.Wrap.("sleep-for" <: float @-> return nil) in
  fun span -> Value.Private.run_outside_async (fun () -> sleep_for (span |> to_seconds))
;;
