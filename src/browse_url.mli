(** Pass a URL to a WWW browser. Ecaml bindings for [browse-url.el] *)

open! Core
open! Async_kernel
open! Import

module Url : sig
  type t = string

  val type_ : t Value.Type.t
  val t : t Value.Type.t
end

(** [(describe-variable 'browse-url-browser-function)] *)
val browser_function : Symbol.t Customization.t

(** [(describe-function 'browse-url)] *)
val browse_url : Url.t -> unit Deferred.t

(** [(describe-function 'browse-url-chrome)] *)
val browse_url_chrome : Url.t -> unit

module Private : sig
  (* If true, actually call browse-url, even when [am_running_test]. *)
  val actually_browse_in_test : bool ref
end
