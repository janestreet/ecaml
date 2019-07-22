open! Core_kernel
open! Import

module Url = struct
  include String

  let type_ = Value.Type.string
  let t = type_
end

let browse_url = Funcall.("browse-url" <: Url.t @-> return nil)
let browse_url_chrome = Funcall.("browse-url-chrome" <: Url.t @-> return nil)
