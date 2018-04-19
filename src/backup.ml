open! Core_kernel
open! Import

module Q = struct
  include Q
  let make_backup_files                = "make-backup-files"                |> Symbol.intern
end

let make_backup_files = Var.create Q.make_backup_files Value.Type.bool
