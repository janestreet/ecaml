(* [window0.ml] is split out from [window.ml] so we can refer to [Window0] in
   [Buffer]. *)

open! Core_kernel
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

type window = t [@@deriving sexp_of]

module Edges = struct
  type t =
    { bottom : int
    ; left : int
    ; right : int
    ; top : int
    }
  [@@deriving sexp_of]

  include Valueable.Remove_t
      ((val Valueable.of_type
              (Value.Type.(
                 map
                   (tuple int (tuple int (tuple int (tuple int unit))))
                   ~name:[%sexp "Window.Tree.Position_and_size.t"])
                 ~of_:(fun (left, (top, (right, (bottom, ())))) ->
                   { bottom; left; right; top })
                 ~to_:(fun { bottom; left; right; top } ->
                   left, (top, (right, (bottom, ())))))))
end

module Tree = struct
  module Direction = struct
    module T = struct
      type t =
        | Left_to_right
        | Top_to_bottom
      [@@deriving enumerate, sexp_of]
    end

    include T

    let is_top_to_bottom = function
      | Left_to_right -> false
      | Top_to_bottom -> true
    ;;

    include Valueable.Remove_t
        ((val Valueable.of_type
                (Value.Type.enum
                   [%sexp "Window.Tree.Direction.t"]
                   (module T)
                   (is_top_to_bottom >> Value.of_bool))))
  end

  type t =
    | Combination of
        { children : t list
        ; direction : Direction.t
        ; edges : Edges.t
        }
    | Window of window
  [@@deriving sexp_of]

  let tuple_type = Value.Type.(tuple Direction.t (tuple Edges.t (list value)))

  let rec of_value_exn value =
    match T.is_in_subtype value with
    | true -> Window (T.of_value_exn value)
    | false ->
      let direction, (edges, children) = Value.Type.of_value_exn tuple_type value in
      let children = List.map children ~f:of_value_exn in
      Combination { children; direction; edges }
  ;;

  let rec to_value = function
    | Window window -> T.to_value window
    | Combination { children; direction; edges } ->
      Value.Type.to_value tuple_type (direction, (edges, List.map children ~f:to_value))
  ;;

  let type_ =
    Value.Type.create [%message "Window.Tree.t"] [%sexp_of: t] of_value_exn to_value
  ;;

  let t = type_

  let parent_exn t window =
    let rec aux t ~parent =
      match t with
      | Window window' ->
        (match T.equal window window' with
         | true -> Some parent
         | false -> None)
      | Combination { children; direction = _; edges = _ } ->
        List.find_map children ~f:(aux ~parent:t)
    in
    match aux t ~parent:t with
    | Some t -> t
    | None -> raise_s [%message "Window not in this tree." (window : window) ~_:(t : t)]
  ;;
end
