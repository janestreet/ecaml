open! Core_kernel
open! Import

module Q = struct
  include Q
  let cancel_timer                     = "cancel-timer"                     |> Symbol.intern
  let memq                             = "memq"                             |> Symbol.intern
  let run_at_time                      = "run-at-time"                      |> Symbol.intern
  let sit_for                          = "sit-for"                          |> Symbol.intern
  let sleep_for                        = "sleep-for"                        |> Symbol.intern
  let timer_list                       = "timer-list"                       |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "timer"
    let here = [%here]
    let is_in_subtype = Value.is_timer
  end)

let timer_list = Var.create Q.timer_list Value.Type.(list type_)

let all_scheduled () = Current_buffer.value_exn timer_list

let is_scheduled t =
  Symbol.funcall2
    Q.memq
    (t |> to_value)
    (Current_buffer.value_exn { symbol = Q.timer_list; type_ = Value.Type.value })
  |> Value.to_bool
;;

let to_seconds span = span |> Time_ns.Span.to_sec |> Value.of_float

let run_after ?repeat span f =
  Symbol.funcall3 Q.run_at_time
    (span |> to_seconds)
    (match repeat with
     | None -> Value.nil
     | Some span -> span |> to_seconds)
    (Function.create [%here] ~args:[] (fun _ -> f (); Value.nil)
     |> Function.to_value)
  |> of_value_exn
;;

let run_after_i ?repeat span f = ignore (run_after ?repeat span f : t)

let cancel t = Symbol.funcall1_i Q.cancel_timer (t |> to_value)

let sit_for ?(redisplay = true) span =
  Symbol.funcall2_i Q.sit_for
    (span |> to_seconds)
    (not redisplay |> Value.of_bool)
;;

let sleep_for span = Symbol.funcall1_i Q.sleep_for (span |> to_seconds)
