(** Pass a URL to a WWW browser.  Ecaml bindings for [browse-url.el] *)

open! Core_kernel
open! Import

module Url : sig
  type t = string
end

val browse_url : Url.t -> unit
