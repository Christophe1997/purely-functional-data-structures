open Core

module type Set = sig
  type t
  type elem

  val empty : t
  val insert : elem -> t -> t
  val member : elem -> t -> bool
    
  val fromOrdList : elem list -> t
end

module Make (Elem : Comparable) : (Set with type elem = Elem.t) = struct
  type elem = Elem.t
  type color = Red | Black
  type t = Empty | Node of (color * t * elem * t)

  let empty = Empty

  let rec member x = function
    | Empty -> false
    | Node (_, left, v, right) ->
      if Elem.compare x v < 0 
      then member x left
      else if Elem.compare x v > 0
      then member x right
      else true

  let balance color left v right = match color, left, v, right with
    | Black, (Node (Red, Node (Red, a, x, b), y, c)), z, d       (* left-left case   *)
    | Black, (Node (Red, a, x, Node (Red, b, y, c))), z, d       (* left-right case  *)
    | Black, a, x, (Node (Red, Node (Red, b, y, c), z, d))       (* right-left case  *)
    | Black, a, x, (Node (Red, b, y, Node (Red, c, z, d))) ->    (* right-right case *)
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | _ as tuple -> Node tuple

  let insert x tree = 
    let rec aux = function
      | Empty -> Node (Red, Empty, x, Empty)
      | Node (color, left, v, right) ->
        if Elem.compare x v < 0
        then balance color (aux left) v right
        else if Elem.compare x v > 0
        then balance color left v (aux right)
        else tree
    in match (aux tree) with
    | Node (_, left, v, right) -> Node (Black, left, v, right)
    | Empty -> Empty  (* have no sense *)

  let fromOrdList xs = 
    let rec aux ret = function
      | [] -> ret
      | [x] -> insert x ret
      | x1 :: (x2 :: tl2 as tl1) -> 
        if x1 = x2 
        then aux ret tl1
        else aux (insert x2 (insert x1 ret)) tl2
    in
    aux empty xs
end

