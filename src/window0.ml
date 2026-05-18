(* [window0.ml] is split out from [window.ml] so we can refer to [Window0] in [Buffer]. *)

open! Core
open! Import0

module T = struct
  include Value.Make_subtype (struct
      let name = "window"
      let here = [%here]
      let is_in_subtype = Value.is_window
    end)

  let equal = eq
end

include T
