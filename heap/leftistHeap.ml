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
  type t = 
    | Leaf
    | Node of int * elem * t * t  (* rank * value * left * right *)

  let empty = Leaf

  let isEmpty = function
    | Leaf -> true
    | _ -> false

  let rank = function
    | Leaf -> 0
    | Node (r, _, _, _) -> r

  let makeTree x h1 h2 = 
    if rank h1 >= rank h2
    then Node (rank h2 + 1, x, h1, h2)
    else Node (rank h1 + 1, x, h2, h1)

  let rec merge h1 h2 = match h1, h2 with
    | (_ as h), Leaf | Leaf, (_ as h) -> h
    | Node (_, x1, left1, right1), Node (_, x2, left2, right2) ->
      if Elem.compare x1 x2 <= 0
      then makeTree x1 left1 (merge right1 h2)
      else makeTree x2 left2 (merge h1 right2)

  let insert x h = merge (Node (1, x, Leaf, Leaf)) h 

  exception Empty

  let findMin = function
    | Node (_, x, _, _) -> x
    | _ -> raise Empty

  let deleteMin = function 
    | Node (_, x, left, right) -> merge left right
    | _ -> raise Empty

  let fromList ls = 
    let rec aux ret = function 
      | [] -> ret
      | hd :: tl -> aux (insert hd ret) tl
    in
    aux empty ls

end

