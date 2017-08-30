(** A feature name is a symbol that stands for a collection of functions, variables, etc.
    The file that defines them should "provide" the feature.  Another program that uses
    them may ensure they are defined by "requiring" the feature.  This loads the file of
    definitions if it hasn't been loaded already.

    [(Info-goto-node "(elisp)Named Features")]. *)

open! Core_kernel
open! Import

(** [(describe-function 'provide)] *)
val provide : Symbol.t -> unit

(** [(describe-function 'require)] *)
val require : Symbol.t -> unit

(** [(describe-function 'featurep)] *)
val is_provided : Symbol.t -> bool

(** [(describe-variable 'features)] *)
val all_provided : unit -> Symbol.t list
