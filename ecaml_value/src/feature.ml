open! Core_kernel
open! Import

module Q = struct
  let require = "require" |> Symbol.intern
end

type t = Symbol.t

let require =
  let seen = Hashtbl.create (module String) in
  fun t ->
    let name = Symbol.name t in
    let seen = Hashtbl.find_or_add seen name ~default:(fun () -> ref []) in
    if not (List.mem !seen t ~equal:Symbol.equal)
    then (
      seen := t :: !seen;
      Symbol.funcall1_i Q.require (t |> Symbol.to_value))
;;
