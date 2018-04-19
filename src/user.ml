open! Core_kernel
open! Import

module Q = struct
  include Q
  let group_gid                        = "group-gid"                        |> Symbol.intern
  let group_real_gid                   = "group-real-gid"                   |> Symbol.intern
  let system_groups                    = "system-groups"                    |> Symbol.intern
  let system_users                     = "system-users"                     |> Symbol.intern
  let user_full_name                   = "user-full-name"                   |> Symbol.intern
  let user_login_name                  = "user-login-name"                  |> Symbol.intern
  let user_real_login_name             = "user-real-login-name"             |> Symbol.intern
  let user_real_uid                    = "user-real-uid"                    |> Symbol.intern
  let user_uid                         = "user-uid"                         |> Symbol.intern
end

let login_name () = Symbol.funcall0 Q.user_login_name |> Value.to_utf8_bytes_exn

let real_login_name () = Symbol.funcall0 Q.user_real_login_name |> Value.to_utf8_bytes_exn

let system_user_names () =
  Symbol.funcall0 Q.system_users |> Value.Type.(list string).of_value_exn
;;

let system_group_names () =
  Symbol.funcall0 Q.system_groups |> Value.Type.(list string).of_value_exn
;;

let full_name () = Symbol.funcall0 Q.user_full_name |> Value.to_utf8_bytes_exn

let nullary_int q () = Symbol.funcall0 q |> Value.to_int_exn

let uid      = nullary_int Q.user_uid
let real_uid = nullary_int Q.user_real_uid
let gid      = nullary_int Q.group_gid
let real_gid = nullary_int Q.group_real_gid

let initialize () =
  Function.defun [%here] ("ecaml-test-user-module" |> Symbol.intern)
    ~interactive:""
    ~args:[]
    (fun _ ->
       Echo_area.message_s [%message
         ""
           (login_name () : string)
           (real_login_name () : string)
           (uid      () : int)
           (real_uid () : int)
           (gid      () : int)
           (real_gid () : int)
           (system_user_names () : string list)
           (system_group_names () : string list)];
       Value.nil);
;;
