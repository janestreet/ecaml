open! Core
open! Async_kernel
open! Import

module Url = struct
  include String

  let type_ = Value.Type.string
  let t = type_
end

let actually_browse_in_test = ref false
let browser_function = Customization.Wrap.("browse-url-browser-function" <: Symbol.t)

let browse_url =
  let browse_url = Funcall.Wrap.("browse-url" <: Url.t @-> return nil) in
  fun url ->
    if am_running_test && not !actually_browse_in_test
    then (
      message_s [%sexp "Would browse url", (url : Url.t)];
      return ())
    else
      (* browser function may be implemented in Async Ecaml *)
      Async_ecaml.Private.run_outside_async1 browse_url url
;;

let browse_url_chrome = Funcall.Wrap.("browse-url-chrome" <: Url.t @-> return nil)

module Private = struct
  let actually_browse_in_test = actually_browse_in_test
end
