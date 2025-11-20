open! Core
open! Async_kernel
open! Import0

include (
  String :
  sig
    type t = string [@@deriving sexp_of]

    include
      Comparable.S
      with type t := t
      with type comparator_witness = String.comparator_witness

    include Hashable.S with type t := t
  end)

include Valueable.Make (struct
    type nonrec t = t

    let type_ = Value.Type.(map_id string) [%message "filename"]
  end)

let is_absolute = Funcall.Wrap.("file-name-absolute-p" <: t @-> return bool)
let extension = Funcall.Wrap.("file-name-extension" <: t @-> return (nil_or string))

let extension_exn t =
  match extension t with
  | Some x -> x
  | None -> raise_s [%message "Filename.extension_exn" ~_:(t : t)]
;;

let nondirectory = Funcall.Wrap.("file-name-nondirectory" <: t @-> return string)
let of_directory = Funcall.Wrap.("directory-file-name" <: string @-> return string)
let sans_extension = Funcall.Wrap.("file-name-sans-extension" <: t @-> return string)
let to_directory = Funcall.Wrap.("file-name-as-directory" <: t @-> return string)
let directory = Funcall.Wrap.("file-name-directory" <: t @-> return (nil_or string))

let directory_exn t =
  match directory t with
  | Some t -> t
  | None ->
    raise_s
      [%message
        "[Filename.directory_exn] of filename that has no directory" ~filename:(t : t)]
;;

let file_relative_name = Funcall.Wrap.("file-relative-name" <: t @-> t @-> return t)
let make_relative t ~relative_to = file_relative_name t relative_to
let expand_file_name = Funcall.Wrap.("expand-file-name" <: t @-> nil_or t @-> return t)

let expand t ~in_dir =
  let in_dir =
    match in_dir with
    | `Default_directory_in_current_buffer -> None
    | `This dir -> Some dir
  in
  expand_file_name t in_dir
;;

let read =
  let read_file_name = Funcall.Wrap.("read-file-name" <: string @-> return t) in
  fun ~prompt -> Value.Private.run_outside_async (fun () -> read_file_name prompt)
;;

let absolute_t =
  Value.Type.(map t)
    ~name:[%message "absolute filename"]
    ~of_:(fun s ->
      if is_absolute s
      then (
        (* Emacs considers paths like [~/src] to be acceptable "absolute" paths, so we
           need to expand them. *)
        let s =
          if String.is_prefix s ~prefix:"~"
          then expand s ~in_dir:`Default_directory_in_current_buffer
          else s
        in
        File_path.Absolute.of_string s)
      else raise_s [%message "Not an absolute path" ~_:(s : Filename.t)])
    ~to_:File_path.Absolute.to_string
;;

let relative_t =
  Value.Type.(map t)
    ~name:[%message "relative filename"]
    ~of_:File_path.Relative.of_string
    ~to_:File_path.Relative.to_string
;;
