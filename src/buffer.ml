open! Core_kernel
open! Async_kernel
open! Import0

module Q = struct
  include Q

  let visible = "visible" |> Symbol.intern
end

module Process = Process0
module Window = Window0
include Buffer0

type buffer = t [@@deriving sexp_of]

let name = Funcall.Wrap.("buffer-name" <: t @-> return (nil_or string))

module Compare_by_name = struct
  module T = struct
    type t = buffer [@@deriving sexp_of]

    let compare = Comparable.lift [%compare: string option] ~f:name
  end

  include T
  include Comparable.Make_plain (T)
end

let file_name = Funcall.Wrap.("buffer-file-name" <: t @-> return (nil_or string))
let process = Funcall.Wrap.("get-buffer-process" <: t @-> return (nil_or Process.t))

let process_exn t =
  match process t with
  | Some process -> process
  | None -> raise_s [%message "buffer does not have a process" ~_:(t : t)]
;;

let all_live = Funcall.Wrap.("buffer-list" <: nullary @-> return (list t))
let get_buffer = Funcall.Wrap.("get-buffer" <: string @-> return (nil_or t))
let find ~name = get_buffer name

let find_exn ~name =
  match find ~name with
  | Some buffer -> buffer
  | None -> raise_s [%message "no buffer named" ~_:(name : string)]
;;

let get_file_buffer = Funcall.Wrap.("get-file-buffer" <: string @-> return (nil_or t))
let find_visiting ~file = get_file_buffer file
let get_buffer_create = Funcall.Wrap.("get-buffer-create" <: string @-> return t)
let find_or_create ~name = get_buffer_create name

let get_buffer_window_list =
  Funcall.Wrap.(
    "get-buffer-window-list" <: t @-> bool @-> value @-> return (list Window.t))
;;

let displayed_in ?(current_frame_only = false) t =
  get_buffer_window_list
    t
    false
    (if current_frame_only then Value.nil else Q.visible |> Symbol.to_value)
;;

let display = Funcall.Wrap.("display-buffer" <: t @-> return (nil_or Window.t))
let display_i t = ignore (display t : Window0.t option)

let buffer_local_value =
  Funcall.Wrap.("buffer-local-value" <: Symbol.t @-> t @-> return value)
;;

let buffer_local_value t (var : _ Var.t) =
  buffer_local_value var.symbol t |> Value.Type.of_value_exn var.type_
;;

let buffer_local_variables =
  Funcall.Wrap.("buffer-local-variables" <: t @-> return (list value))
;;

let buffer_local_variables t =
  buffer_local_variables t
  |> List.map ~f:(fun value ->
    if Value.is_symbol value
    then value |> Symbol.of_value_exn, None
    else Value.car_exn value |> Symbol.of_value_exn, Some (Value.cdr_exn value))
;;

let find_file_noselect =
  let f = Funcall.Wrap.("find-file-noselect" <: string @-> return t) in
  fun filename -> Value.Private.run_outside_async [%here] (fun () -> f filename)
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

  let type_ =
    Value.Type.create
      [%message "Which_buffers.t"]
      [%sexp_of: t]
      (fun _ -> raise_s [%message [%here]])
      to_value
  ;;

  let t = type_
end

let save_some =
  let save_some_buffers =
    Funcall.Wrap.("save-some-buffers" <: bool @-> Which_buffers.t @-> return nil)
  in
  fun ?(query = true) ?(which_buffers = Which_buffers.File_visiting) () ->
    Value.Private.run_outside_async [%here] (fun () ->
      try save_some_buffers (not query) which_buffers with
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

let revert =
  let revert_buffer =
    Funcall.Wrap.("revert-buffer" <: bool @-> bool @-> bool @-> return bool)
  in
  fun ?(confirm = false) t ->
    let noconfirm = not confirm in
    Value.Private.run_outside_async [%here] ~allowed_in_background:noconfirm (fun () ->
      Current_buffer0.set_temporarily Sync t ~f:(fun () ->
        ignore (revert_buffer false noconfirm false : bool)))
;;

let kill_buffer_query_functions =
  Var.Wrap.("kill-buffer-query-functions" <: list Function.t)
;;

let modified_tick = Funcall.Wrap.("buffer-modified-tick" <: t @-> return Modified_tick.t)

let chars_modified_tick =
  Funcall.Wrap.("buffer-chars-modified-tick" <: t @-> return Modified_tick.t)
;;
