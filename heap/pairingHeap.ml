open Core

module type Heap = sig
  type t
  type elem

  exception Empty

  val empty : t
  val isEmpty : t -> bool

  val insert : elem -> t -> t
  val merge : t -> t -> t

  val findMin : t -> elem
  val deleteMin : t -> t

  val fromList : elem list -> t
end

module Make (Elem : Comparable) : (Heap with type elem = Elem.t) = struct
  type elem = Elem.t
  type t = Leaf | Node of elem * t list

  exception Empty

  let empty = Leaf

  let isEmpty = function
    | Leaf -> true
    | _ -> false

  let merge h1 h2 = match h1, h2 with
    | Leaf, _ as h | _ as h, Leaf -> h
    | Node (v1, hs1), Node (v2, hs2) ->
      if Elem.compare v1 v2 <= 0
      then Node (v1, h2 :: hs1)
      else Node (v2, h1 :: hs2)

  let insert x h = merge (Node (x, [])) h

  let mergePairs = function
    | [] -> Leaf
    | [h] -> h
    | h1 :: h2 :: hs -> merge (merge h1 h2) (mergePairs hs)

  let findMin = function
    | Leaf -> raise Empty
    | Node (x, hs) -> x

  let deleteMin = function
    | Leaf -> raise Empty
    | Node (x, hs) -> mergePairs hs 

end

