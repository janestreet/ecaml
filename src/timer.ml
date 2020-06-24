open! Core_kernel
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
  Funcall.Wrap.("run-at-time" <: float @-> nil_or float @-> Symbol.t @-> return t)
;;

let run_after ?repeat here span ~f ~name =
  Defun.defun_nullary_nil name here f;
  run_at_time (span |> to_seconds) (repeat |> Option.map ~f:to_seconds) name
;;

let run_after_i ?repeat here span ~f ~name =
  ignore (run_after here ?repeat span ~f ~name : t)
;;

let cancel = Funcall.Wrap.("cancel-timer" <: t @-> return nil)

let sit_for =
  let sit_for = Funcall.Wrap.("sit-for" <: float @-> bool @-> return nil) in
  fun ?(redisplay = true) span ->
    Value.Private.run_outside_async [%here] (fun () ->
      sit_for (span |> to_seconds) (not redisplay))
;;

let sleep_for =
  let sleep_for = Funcall.Wrap.("sleep-for" <: float @-> return nil) in
  fun span ->
    Value.Private.run_outside_async [%here] (fun () -> sleep_for (span |> to_seconds))
;;
