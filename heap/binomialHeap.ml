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
end

module Make (Elem : Comparable) : (Heap with type elem = Elem.t) = struct
  type elem = Elem.t
  type tree = Node of int * elem * tree list
  type t = tree list
    
  exception Empty

  let empty = []
  let isEmpty = function 
    | [] -> true
    | _ -> false
    
  let link t1 t2 = match t1, t2 with
    | Node (r, v1, ts1), Node (_, v2, ts2) -> 
      if Elem.compare v1 v2 <= 0
      then Node (r + 1, v1, t2 :: ts1)
      else Node (r + 1, v2, t1 :: ts2)

  let rank = function
    | Node (r, _, _) -> r

  let root = function
    | Node (_, v, _) -> v

  let rec insertTree t = function
    | [] -> [t]
    | hd :: tl as ts ->  
      if rank t < rank hd
      then t :: ts
      else insertTree (link t hd) tl

  let insert x ts = insertTree (Node (0, x, [])) ts

  let rec merge ts1 ts2 = match ts1, ts2 with
    | [], (_ as ts) | (_ as ts), [] -> ts
    | hd1 :: tl1, hd2 :: tl2 -> 
      if rank hd1 < rank hd2
      then hd1 :: (merge tl1 ts2)
      else if rank hd2 < rank hd1
      then hd2 :: (merge tl2 ts1)
      else insertTree (link hd1 hd2) (merge tl1 tl2)

  let rec removeMinTree = function
    | [] -> raise Empty
    | [t] -> t, []
    | hd :: tl -> match (removeMinTree tl) with
      | t, ts -> 
        if Elem.compare (root hd) (root t) <= 0 
        then hd, tl
        else t, hd :: ts

  let findMin ts = match (removeMinTree ts) with
    | t, _ -> root t

  let deleteMin ts = match (removeMinTree ts) with
    | Node (_, _, ts1), ts2 -> merge (List.rev ts1) ts2
end

