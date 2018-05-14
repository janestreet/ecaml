(** The name of a file.  Functions in this module work directly on names, but do not read
    from or write to the filesystem.

    [(Info-goto-node "(elisp)File Names")] *)

open! Core_kernel
open! Import0

type t = string [@@deriving compare, sexp_of]

include Comparable.S
  with type t := t
  with type comparator_witness = String.comparator_witness
include Valueable.S with type t := t

(** [(describe-function 'file-name-directory)]
    [(Info-goto-node "(elisp)File Name Components")] *)
val directory     : t -> t option
val directory_exn : t -> t

(** [(describe-function 'file-name-nondirectory)]
    [(Info-goto-node "(elisp)File Name Components")] *)
val nondirectory : t -> t

(** [(describe-function 'file-name-extension)]
    [(Info-goto-node "(elisp)File Name Components")] *)
val extension_exn : t -> string

(** [(describe-function 'file-name-sans-extension)]
    [(Info-goto-node "(elisp)File Name Components")] *)
val sans_extension : t -> string

(** [(describe-function 'file-name-as-directory)]
    [(Info-goto-node "(elisp)Directory Names")] *)
val to_directory : t -> t

(** [(describe-function 'directory-file-name)]
    [(Info-goto-node "(elisp)Directory Names")] *)
val of_directory : t -> t

(** [(describe-function 'file-name-absolute-p)]
    [(Info-goto-node "(elisp)Relative File Names")] *)
val is_absolute : t -> bool

(** [(describe-function 'file-relative-name)]
    [(Info-goto-node "(elisp)Relative File Names")] *)
val make_relative : t -> relative_to:t -> t

(** [(describe-function 'expand-file-name)]
    [(Info-goto-node "(elisp)File Name Expansion")] *)
val expand : t -> in_dir:t -> t
