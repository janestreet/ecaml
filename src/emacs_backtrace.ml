open! Core_kernel
open! Import

let standard_output = Var.Wrap.("standard-output" <: value)
let backtrace = Funcall.Wrap.("backtrace" <: nullary @-> return nil)

let get () =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    Current_buffer.set_value_temporarily
      Sync
      standard_output
      (Current_buffer.get () |> Buffer.to_value)
      ~f:(fun () ->
        backtrace ();
        Current_buffer.contents () |> Text.to_utf8_bytes))
;;
