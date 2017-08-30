open! Core_kernel
open! Import

module Process = Process0
module Window  = Window0

include Value.Make_subtype (struct
    let name = "buffer"
    let here = [%here]
    let is_in_subtype = Value.is_buffer
  end)

let equal = eq

module Private = struct
  let current () = Symbol.funcall0 Q.current_buffer |> of_value_exn
end

let is_live t = Generated_bindings.buffer_live_p (t |> to_value)

let name t =
  let result = Symbol.funcall1 Q.buffer_name (t |> to_value) in
  if Value.is_nil result
  then None
  else Some (result |> Value.to_utf8_bytes_exn)
;;

let file_name t =
  let result = Symbol.funcall1 Q.buffer_file_name (t |> to_value) in
  if Value.is_nil result
  then None
  else Some (result |> Value.to_utf8_bytes_exn)
;;

let process t =
  let result = Symbol.funcall1 Q.get_buffer_process (t |> to_value) in
  if Value.is_nil result
  then None
  else Some (result |> Process.of_value_exn)
;;

let all_live () =
  Symbol.funcall0 Q.buffer_list |> Value.to_list_exn ~f:of_value_exn
;;

let create ~name =
  Symbol.funcall1 Q.generate_new_buffer (name |> Value.of_utf8_bytes)
  |> of_value_exn
;;

let find ~name =
  let v = Symbol.funcall1 Q.get_buffer (name |> Value.of_utf8_bytes) in
  if Value.is_nil v
  then None
  else Some (v |> of_value_exn)
;;

let find_or_create ~name =
  Symbol.funcall1 Q.get_buffer_create (name |> Value.of_utf8_bytes)
  |> of_value_exn
;;

let kill t = Symbol.funcall1_i Q.kill_buffer (t |> to_value)

let displayed_in t =
  Symbol.funcall1 Q.get_buffer_window_list (t |> to_value)
  |> Value.to_list_exn ~f:Window.of_value_exn
;;

let display t = Symbol.funcall1_i Q.display_buffer (t |> to_value)
