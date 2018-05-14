open! Core_kernel
open! Import0

module Q = struct
  include Q
  let buffer_list            = "buffer-list"            |> Symbol.intern
  let buffer_local_value     = "buffer-local-value"     |> Symbol.intern
  let buffer_name            = "buffer-name"            |> Symbol.intern
  let display_buffer         = "display-buffer"         |> Symbol.intern
  let find_file_noselect     = "find-file-noselect"     |> Symbol.intern
  let generate_new_buffer    = "generate-new-buffer"    |> Symbol.intern
  let get_buffer             = "get-buffer"             |> Symbol.intern
  let get_buffer_create      = "get-buffer-create"      |> Symbol.intern
  let get_buffer_process     = "get-buffer-process"     |> Symbol.intern
  let get_buffer_window_list = "get-buffer-window-list" |> Symbol.intern
  let get_file_buffer        = "get-file-buffer"        |> Symbol.intern
  let save_some_buffers      = "save-some-buffers"      |> Symbol.intern
end

module Process = Process0
module Window  = Window0

include Buffer0

type buffer = t

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

let find_visiting ~file =
  let v = Symbol.funcall1 Q.get_file_buffer (file |> Value.of_utf8_bytes) in
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

let buffer_local_value t (var : _ Var.t) =
  Symbol.funcall2 Q.buffer_local_value (var |> Var.symbol_as_value) (t |> to_value)
  |> var.type_.of_value_exn
;;

let buffer_local_variables t =
  Symbol.funcall1 Q.buffer_local_variables (t |> to_value)
  |> Value.to_list_exn ~f:(fun value ->
    if Value.is_symbol value
    then (value |> Symbol.of_value_exn, None)
    else (Value.car_exn value |> Symbol.of_value_exn, Some (Value.cdr_exn value)))
;;

let find_file_noselect filename =
  Symbol.funcall1 Q.find_file_noselect (filename |> Value.of_utf8_bytes)
  |> of_value_exn
;;

module Which_buffers = struct
  type t =
    | File_visiting
    | These of (buffer -> bool)
  [@@deriving sexp_of]

  let to_value = function
    | File_visiting -> Value.nil
    | These f ->
      Function.create [%here] ~args:[]
        (fun _ -> f (Current_buffer0.get ()) |> Value.of_bool)
      |> Function.to_value
  ;;
end

let save_some ?(query = true) ?(which_buffers = Which_buffers.File_visiting) () =
  Symbol.funcall2_i Q.save_some_buffers
    (not query |> Value.of_bool)
    (which_buffers |> Which_buffers.to_value)
;;
