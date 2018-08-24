(* [Ecaml_filename] exists so that other modules in this directory can [open
   Ecaml_filename] to get [Ecaml.Filename] rather than [Core_kernel.Filename]. *)
module Filename = Filename
