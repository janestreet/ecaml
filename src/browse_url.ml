open! Core_kernel
open! Import

module Url = struct
  include String

  let type_ = Value.Type.string
  let t = type_
end

let browse_url =
  let browse_url = Funcall.Wrap.("browse-url" <: Url.t @-> return nil) in
  fun url ->
    if am_running_test
    then message_s [%sexp "Would browse url", (url : Url.t)]
    else browse_url url
;;

let browse_url_chrome = Funcall.Wrap.("browse-url-chrome" <: Url.t @-> return nil)
