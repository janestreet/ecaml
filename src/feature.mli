(** A feature name is a symbol that stands for a collection of functions, variables, etc.
    The file that defines them should "provide" the feature.  Another program that uses
    them may ensure they are defined by "requiring" the feature.  This loads the file of
    definitions if it hasn't been loaded already.

    [(Info-goto-node "(elisp)Named Features")]. *)

open! Core_kernel
open! Import0

include module type of struct
  include Ecaml_value.Feature
end

(** [(describe-function 'provide)] *)
val provide : t -> unit
[@@deprecated
  "[since 2018-06] Requires additional initialization.  Use [Ecaml.provide] instead."]

(** [(describe-function 'featurep)] *)
val is_provided : t -> bool

(** [(describe-variable 'features)] *)
val all_provided : unit -> t list
