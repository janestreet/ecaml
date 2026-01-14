open! Core
open! Import

module Q = struct
  let propertize = ":propertize" |> Symbol.intern
end

module Format = struct
  include Value.Make_subtype (struct
      let name = "mode-line-format"
      let here = [%here]
      let is_in_subtype _ = true
    end)

  let in_buffer = Buffer_local.Wrap.("mode-line-format" <: t)
  let header_line_in_buffer = Buffer_local.Wrap.("header-line-format" <: t)
  let empty = Value.nil |> of_value_exn

  let string_verbatim =
    let pattern = String.Search_pattern.create "%" in
    fun string ->
      let quoted = String.Search_pattern.replace_all pattern ~in_:string ~with_:"%%" in
      Value.of_utf8_bytes quoted |> of_value_exn
  ;;

  let propertize t properties =
    Value.list
      ([ Q.propertize |> Symbol.to_value; to_value t ]
       @ Text.Property.to_property_list properties)
    |> of_value_exn
  ;;
end

let text = Funcall.Wrap.("format-mode-line" <: Format.t @-> return Text.t)
