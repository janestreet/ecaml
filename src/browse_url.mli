(** Pass a URL to a WWW browser.  Ecaml bindings for [browse-url.el] *)

open! Core_kernel
open! Import

module Url : sig
  type t = string

  val type_ : t Value.Type.t
  val t : t Value.Type.t
end

(** [(describe-function 'browse-url)] *)
val browse_url : Url.t -> unit

(** [(describe-function 'browse-url-chrome)] *)
val browse_url_chrome : Url.t -> unit
