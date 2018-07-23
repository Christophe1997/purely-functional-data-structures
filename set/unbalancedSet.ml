open Core

module type Set = sig
  type t
  type elem 

  val empty : t 
  val insert : elem -> t -> t
  val member : elem -> t -> bool
end

module Make (Elem : Comparable) : (Set with type elem = Elem.t) = struct
  type elem = Elem.t
  type t = 
    | Leaf 
    | Node of t * elem * t

  let empty = Leaf

  exception AlreadyInSet

  let rec insert x tree = 
    let aux = function
      | Leaf -> Node (Leaf, x, Leaf)
      | Node (left, v, right) ->
        if Elem.compare x v < 0 
        then Node (insert x left, v, right)
        else if Elem.compare x v > 0
        then Node (left, v, insert x right)
        else raise AlreadyInSet 
    in
    try 
      aux tree
    with AlreadyInSet -> tree


  let rec member x tree = 
    let rec aux candidate = function
      | Leaf -> 
        (match candidate with
         | None -> false
         | Some v -> Elem.compare x v = 0)
      | Node (left, v, right) ->
        if Elem.compare x v < 0
        then aux candidate left
        else aux (Some v) right
    in
    aux None tree
end
