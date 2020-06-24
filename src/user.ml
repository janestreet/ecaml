open! Core_kernel
open! Import

let login_name = Funcall.Wrap.("user-login-name" <: nullary @-> return string)
let real_login_name = Funcall.Wrap.("user-real-login-name" <: nullary @-> return string)
let system_user_names = Funcall.Wrap.("system-users" <: nullary @-> return (list string))

let system_group_names =
  Funcall.Wrap.("system-groups" <: nullary @-> return (list string))
;;

let full_name = Funcall.Wrap.("user-full-name" <: nullary @-> return string)
let uid = Funcall.Wrap.("user-uid" <: nullary @-> return int)
let real_uid = Funcall.Wrap.("user-real-uid" <: nullary @-> return int)
let gid = Funcall.Wrap.("group-gid" <: nullary @-> return int)
let real_gid = Funcall.Wrap.("group-real-gid" <: nullary @-> return int)

let () =
  Defun.defun_nullary_nil
    ("ecaml-test-user-module" |> Symbol.intern)
    [%here]
    ~interactive:No_arg
    (fun () ->
       message_s
         [%message
           ""
             (login_name () : string)
             (real_login_name () : string)
             (uid () : int)
             (real_uid () : int)
             (gid () : int)
             (real_gid () : int)
             (system_user_names () : string list)
             (system_group_names () : string list)])
;;
