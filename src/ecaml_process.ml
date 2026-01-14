(* [Ecaml_process] exists so that other modules in this directory can [open Ecaml_process]
   to get [Ecaml.Process] rather than [Async_unix.Process]. *)
module Process = Process
