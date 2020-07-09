open! Core_kernel
open! Import

module F0 = struct
  let float_time = Funcall.Wrap.("float-time" <: Value.Type.value @-> return float)
end

include Value.Make_subtype (struct
    let name = "time"
    let here = [%here]

    let is_in_subtype value =
      match F0.float_time value with
      | exception _ -> false
      | (_ : float) -> true
    ;;
  end)

let t = type_

let format_time_string =
  Funcall.Wrap.("format-time-string" <: string @-> t @-> nil_or string @-> return string)
;;

let format ?zone t ~format_string =
  format_time_string format_string t (Option.map zone ~f:Time.Zone.name)
;;

let sexp_of_t t = [%sexp (format t ~format_string:"%F %T.%N%z" : string)]
let ( < ) = Funcall.Wrap.("time-less-p" <: t @-> t @-> return bool)
let ( > ) t1 t2 = t2 < t1
let compare t1 t2 = if t1 < t2 then -1 else if t1 > t2 then 1 else 0

let of_time_ns time_ns =
  try
    let nanos = time_ns |> Time_ns.to_int_ns_since_epoch in
    let negate, nanos = if Int.( < ) nanos 0 then true, -nanos else false, nanos in
    let sub_micro_nanos = nanos % 1_000 in
    let picos = sub_micro_nanos * 1_000 in
    let sec_and_micros = (nanos - sub_micro_nanos) / 1_000 in
    let micros = sec_and_micros % 1_000_000 in
    let sec = (sec_and_micros - micros) / 1_000_000 in
    let sec_low = sec land 0xFFFF in
    let sec_high = (sec - sec_low) lsr 16 in
    (if negate
     then [ -sec_high; -sec_low; -micros; -picos ]
     else [ sec_high; sec_low; micros; picos ])
    |> Value.Type.(list int |> to_value)
    |> of_value_exn
  with
  | exn ->
    raise_s
      [%message
        "[Elisp_time.of_time_ns]" (time_ns : Time_ns.Alternate_sexp.t) ~error:(exn : exn)]
;;

let min_time_ns_value = lazy (of_time_ns Time_ns.min_value_for_1us_rounding)
let max_time_ns_value = lazy (of_time_ns Time_ns.max_value_for_1us_rounding)

let unexpected_time_value value =
  raise_s [%message "[Elisp_time] got unexpected time value" (value : Value.t)]
;;

let to_int_ns_since_epoch_exn t =
  let min_time_ns_value = force min_time_ns_value in
  let max_time_ns_value = force max_time_ns_value in
  if t < min_time_ns_value
  then
    raise_s
      [%message
        "[Elisp_time.to_int_ns_since_epoch] got too small time"
          ~_:(t : t)
          (min_time_ns_value : t)];
  if t > max_time_ns_value
  then
    raise_s
      [%message
        "[Elisp_time.to_int_ns_since_epoch] got too large time"
          ~_:(t : t)
          (max_time_ns_value : t)];
  let value = t |> to_value in
  if Value.is_integer value
  then 1_000_000_000 * (value |> Value.to_int_exn)
  else if Value.is_float value
  then Float.iround_nearest_exn (1e9 *. (value |> Value.to_float_exn))
  else (
    let sec_high, sec_low, micros, picos =
      match value |> Value.Type.(list int |> of_value_exn) with
      | exception _ -> unexpected_time_value value
      | [ sec_high ] -> sec_high, 0, 0, 0
      | [ sec_high; sec_low ] -> sec_high, sec_low, 0, 0
      | [ sec_high; sec_low; micros ] -> sec_high, sec_low, micros, 0
      | [ sec_high; sec_low; micros; picos ] -> sec_high, sec_low, micros, picos
      | _ -> unexpected_time_value value
    in
    (((sec_high lsl 16) + sec_low) * 1_000_000_000) + (micros * 1_000) + (picos / 1_000))
;;

let to_time_ns_exn t = t |> to_int_ns_since_epoch_exn |> Time_ns.of_int_ns_since_epoch
