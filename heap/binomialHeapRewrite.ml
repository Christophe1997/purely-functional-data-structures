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
  type tree = Node of elem * tree list
  type t = (int * tree) list

  exception Empty

  let empty = []
  let isEmpty = function
    | [] -> true
    | _ -> false

  let root = function 
    | Node (v, _) -> v

  let link t1 t2 = match t1, t2 with
    | Node (v1, ts1), Node (v2, ts2) ->
      if Elem.compare v1 v2 <= 0
      then Node (v1, t2 :: ts1)
      else Node (v2, t1 :: ts2)

  let rec insertTree (r, t) = function
    | [] -> [r, t]
    | (r1, t1) :: tl as ts ->
      if r < r1
      then (r, t) :: ts
      else insertTree (r + 1, link t t1) tl

  let insert x ts = insertTree (0, Node (x, [])) ts

  let rec merge ts1 ts2 = match ts1, ts2 with
    | [], (_ as ts) | (_ as ts), [] -> ts
    | ((r1, t1) as hd1) :: tl1, ((r2, t2) as hd2) :: tl2 ->
      if r1 < r2
      then hd1 :: (merge tl1 ts2)
      else if r2 < r1
      then hd2 :: (merge tl2 ts1)
      else insertTree (r2 + 1, link t1 t2) (merge tl1 tl2)
  
  let rec removeMinTree = function
    | [] -> raise Empty
    | [t] -> t, []
    | (r1, t1) as hd :: tl -> match (removeMinTree tl) with
      | (r2, t2) as t, ts ->
        if r1 <= r2
        then hd, tl
        else t, hd :: ts

  let findMin ts = match (removeMinTree ts) with
    | (r, t), _ -> root t

  let deleteMin ts = match (removeMinTree ts) with
    | (r, Node (_, ts1)), ts2 -> 
      merge (List.fold_left ts1 ~init:[] ~f:(fun ts t ->
          match ts with
           | [] -> [r - 1, t]
           | (r1, t1) :: tl -> (r1 - 1, t) :: ts)) ts2

end
