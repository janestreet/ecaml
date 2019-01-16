open! Core_kernel
open! Import

module Url = struct
  include String

  let type_ = Value.Type.string
end

module Q = struct
  let browse_url = "browse-url" |> Symbol.intern
  let browse_url_chrome = "browse-url-chrome" |> Symbol.intern
end

module F = struct
  open Funcall

  let browse_url = Q.browse_url <: Url.type_ @-> return nil
  let browse_url_chrome = Q.browse_url_chrome <: Url.type_ @-> return nil
end

let browse_url = F.browse_url
let browse_url_chrome = F.browse_url_chrome
