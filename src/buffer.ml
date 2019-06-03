open! Core_kernel
open! Async_kernel
open! Import0

module Q = struct
  include Q

  let buffer_list = "buffer-list" |> Symbol.intern
  and buffer_local_value = "buffer-local-value" |> Symbol.intern
  and buffer_name = "buffer-name" |> Symbol.intern
  and display_buffer = "display-buffer" |> Symbol.intern
  and find_file_noselect = "find-file-noselect" |> Symbol.intern
  and get_buffer = "get-buffer" |> Symbol.intern
  and get_buffer_create = "get-buffer-create" |> Symbol.intern
  and get_buffer_process = "get-buffer-process" |> Symbol.intern
  and get_buffer_window_list = "get-buffer-window-list" |> Symbol.intern
  and get_file_buffer = "get-file-buffer" |> Symbol.intern
  and revert_buffer = "revert-buffer" |> Symbol.intern
  and save_some_buffers = "save-some-buffers" |> Symbol.intern
  and visible = "visible" |> Symbol.intern
end

module F = struct
  open Funcall
  open Value.Type

  let revert_buffer = Q.revert_buffer <: bool @-> bool @-> bool @-> return bool
end

module Process = Process0
module Window = Window0

include (
  Buffer0 :
    module type of struct
    include Buffer0
  end
  with module Q := Buffer0.Q)

type buffer = t [@@deriving sexp_of]

let is_live t = Generated_bindings.buffer_live_p (t |> to_value)

let name t =
  let result = Symbol.funcall1 Q.buffer_name (t |> to_value) in
  if Value.is_nil result then None else Some (result |> Value.to_utf8_bytes_exn)
;;

module Compare_by_name = struct
  module T = struct
    type t = buffer [@@deriving sexp_of]

    let compare = Comparable.lift [%compare: string option] ~f:name
  end

  include T
  include Comparable.Make_plain (T)
end

let file_name t =
  let result = Symbol.funcall1 Q.buffer_file_name (t |> to_value) in
  if Value.is_nil result then None else Some (result |> Value.to_utf8_bytes_exn)
;;

let process t =
  let result = Symbol.funcall1 Q.get_buffer_process (t |> to_value) in
  if Value.is_nil result then None else Some (result |> Process.of_value_exn)
;;

let process_exn t =
  match process t with
  | Some process -> process
  | None -> raise_s [%message "buffer does not have a process" ~_:(t : t)]
;;

let all_live () = Symbol.funcall0 Q.buffer_list |> Value.to_list_exn ~f:of_value_exn

let find ~name =
  let v = Symbol.funcall1 Q.get_buffer (name |> Value.of_utf8_bytes) in
  if Value.is_nil v then None else Some (v |> of_value_exn)
;;

let find_visiting ~file =
  let v = Symbol.funcall1 Q.get_file_buffer (file |> Value.of_utf8_bytes) in
  if Value.is_nil v then None else Some (v |> of_value_exn)
;;

let find_or_create ~name =
  Symbol.funcall1 Q.get_buffer_create (name |> Value.of_utf8_bytes) |> of_value_exn
;;

let displayed_in ?(current_frame_only = false) t =
  Symbol.funcall3
    Q.get_buffer_window_list
    (t |> to_value)
    Value.nil
    (if current_frame_only then Value.nil else Q.visible |> Symbol.to_value)
  |> Value.to_list_exn ~f:Window.of_value_exn
;;

let display t =
  Symbol.funcall1 Q.display_buffer (t |> to_value)
  |> Value.Type.(nil_or Window0.type_ |> of_value_exn)
;;

let display_i t = ignore (display t : Window0.t option)

let buffer_local_value t (var : _ Var.t) =
  Symbol.funcall2 Q.buffer_local_value (var |> Var.symbol_as_value) (t |> to_value)
  |> Value.Type.of_value_exn var.type_
;;

let buffer_local_variables t =
  Symbol.funcall1 Q.buffer_local_variables (t |> to_value)
  |> Value.to_list_exn ~f:(fun value ->
    if Value.is_symbol value
    then value |> Symbol.of_value_exn, None
    else Value.car_exn value |> Symbol.of_value_exn, Some (Value.cdr_exn value))
;;

let find_file_noselect filename =
  Symbol.funcall1 Q.find_file_noselect (filename |> Value.of_utf8_bytes) |> of_value_exn
;;

let is_internal_or_dead t =
  match name t with
  | None -> true
  | Some name -> String.is_prefix name ~prefix:" "
;;

module Which_buffers = struct
  type t =
    | File_visiting
    | These of (buffer -> bool)
  [@@deriving sexp_of]

  let to_value = function
    | File_visiting -> Value.nil
    | These f ->
      Function.create [%here] ~args:[] (fun _ ->
        let buffer = Current_buffer0.get () in
        try f buffer |> Value.of_bool with
        | exn ->
          raise_s [%message "[Which_buffers.These]" (buffer : buffer) (exn : exn)])
      |> Function.to_value
  ;;
end

let save_some ?(query = true) ?(which_buffers = Which_buffers.File_visiting) () =
  Value.Private.run_outside_async [%here] (fun () ->
    try
      Symbol.funcall2_i
        Q.save_some_buffers
        (not query |> Value.of_bool)
        (which_buffers |> Which_buffers.to_value)
    with
    | exn -> raise_s [%message "[Buffer.save_some]" (exn : exn)])
;;

let with_temp_buffer f =
  let temp_buffer = create ~name:" *temp*" in
  Monitor.protect
    (fun () -> f temp_buffer)
    ~finally:(fun () ->
      match%map Monitor.try_with (fun () -> kill temp_buffer) with
      | _ -> ())
;;

let revert ?(confirm = false) t =
  let noconfirm = not confirm in
  Value.Private.run_outside_async [%here] ~allowed_in_background:noconfirm (fun () ->
    Current_buffer0.set_temporarily t Sync ~f:(fun () ->
      ignore (F.revert_buffer false noconfirm false : bool)))
;;
