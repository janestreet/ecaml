open! Core_kernel
open! Import

module Q = struct
  include Q

  let inhibit_message = "inhibit-message" |> Symbol.intern
end

module Current_buffer = Current_buffer0

let inhibit_message = Var.create Q.inhibit_message Value.Type.bool
let inhibit_messages f = Current_buffer.set_value_temporarily inhibit_message true ~f

let maybe_echo ~echo f =
  if Option.value echo ~default:true then f () else inhibit_messages f
;;

let message ?echo s = maybe_echo ~echo (fun () -> Value.message s)
let messagef ?echo fmt = ksprintf (message ?echo) fmt
let message_s ?echo s = maybe_echo ~echo (fun () -> Value.message_s s)

let wrap_message ?echo message ~f =
  let message = concat [ message; " ... " ] in
  message_s ?echo [%sexp (message : string)];
  match f () with
  | x ->
    message_s ?echo [%sexp (concat [ message; "done" ] : string)];
    x
  | exception exn ->
    message_s ?echo [%sexp (concat [ message; "raised" ] : string)];
    raise exn
;;
