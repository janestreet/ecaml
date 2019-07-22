open! Core_kernel
open! Import
open! Ecaml_filename

module Q = struct
  include Q

  let no_message = "no-message" |> Symbol.intern
end

let exists = Funcall.("file-exists-p" <: Filename.t @-> return bool)
let is_directory = Funcall.("file-directory-p" <: Filename.t @-> return bool)
let is_executable = Funcall.("file-executable-p" <: Filename.t @-> return bool)
let is_readable = Funcall.("file-readable-p" <: Filename.t @-> return bool)
let is_regular = Funcall.("file-regular-p" <: Filename.t @-> return bool)
let is_symlink = Funcall.("file-symlink-p" <: Filename.t @-> return bool)
let is_writable = Funcall.("file-writable-p" <: Filename.t @-> return bool)

let file_in_directory_p =
  Funcall.("file-in-directory-p" <: Filename.t @-> Filename.t @-> return bool)
;;

let is_below file ~dir = file_in_directory_p file dir
let truename = Funcall.("file-truename" <: Filename.t @-> return Filename.t)
let delete = Funcall.("delete-file" <: Filename.t @-> return nil)
let copy_file = Funcall.("copy-file" <: Filename.t @-> Filename.t @-> return nil)
let copy ~src ~dst = copy_file src dst

let rename_file =
  Funcall.("rename-file" <: Filename.t @-> Filename.t @-> bool @-> return nil)
;;

let rename ~src ~dst ~replace_dst_if_exists = rename_file src dst replace_dst_if_exists

let locate_file =
  Funcall.(
    "locate-file"
    <: string
       @-> list string
       @-> nil_or (list string)
       @-> nil_or value
       @-> return (nil_or string))
;;

let locate ?suffixes ?predicate ~filename ~path () =
  locate_file filename path suffixes predicate
;;

let locate_dominating_file =
  Funcall.(
    "locate-dominating-file" <: Filename.t @-> Filename.t @-> return (nil_or Filename.t))
;;

let locate_dominating_file ~above ~basename = locate_dominating_file above basename

let locate_dominating_file_exn ~above ~basename =
  match locate_dominating_file ~above ~basename with
  | Some x -> x
  | None ->
    raise_string [ "Unable to find ["; basename; "] in directory above ["; above; "]." ]
;;

let write_region =
  Funcall.(
    "write-region" <: string @-> unit @-> Filename.t @-> bool @-> Symbol.t @-> return nil)
;;

let write ?(append = false) filename data =
  write_region data () filename append Q.no_message
;;

let ensure_exists filename = write filename "" ~append:true

(* squelch the [Wrote file] message *)

let make_temp_file =
  Funcall.("make-temp-file" <: string @-> bool @-> string @-> return Filename.t)
;;

let make_temp_file ~prefix ~suffix = make_temp_file prefix false suffix

let with_temp_file sync_or_async ~f ~prefix ~suffix =
  let filename = make_temp_file ~prefix ~suffix in
  Sync_or_async.protect
    [%here]
    sync_or_async
    ~f:(fun () -> f filename)
    ~finally:(fun () -> delete filename)
;;

let find_executable = Funcall.("executable-find" <: string @-> return (nil_or string))
