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
  type t = Leaf | Node of t * elem * t

  exception Empty

  let empty = Leaf

  let isEmpty = function
    | Leaf -> true
    | _ -> false

  let rec partition pivot = function
    | Leaf -> Leaf, Leaf
    | Node (left, v, right) as tree ->
      if Elem.compare v pivot <= 0
      then match right with
        | Leaf -> tree, Leaf
        | Node (left1, v1, right1) ->
          if Elem.compare v1 pivot <= 0
          then let small, big = partition pivot right1 in
            Node (Node (left, v, left1), v1, small), big
          else let small, big = partition pivot left1 in
            Node (left, v, small), Node (big, v1, right1)
      else match left with
        | Leaf -> Leaf, tree
        | Node (left1, v1, right1) ->
          if Elem.compare v1 pivot <= 0
          then let small, big = partition pivot right1 in
            Node (left1, v1, small), Node (big, v, right)
          else let small, big = partition pivot left1 in
            small, Node (big, v1, Node (right1, v, right))

  let insert x t =
    let left, right = partition x t in
    Node (left, x, right)

  let rec merge t1 t2 = match t1, t2 with
    | Leaf, _ -> t2
    | Node (left, v, right), _ -> 
      let small, big = partition v t2 in
      Node (merge small left, v, merge big right)

  let rec findMin = function
    | Leaf -> raise Empty
    | Node (Leaf, v, _) -> v
    | Node (left, v, _) -> findMin left

  let rec deleteMin = function
    | Leaf -> raise Empty
    | Node (Leaf, v, right) -> right
    | Node (Node (Leaf, v1, right1), v2, right2) -> Node (right1, v2, right2)
    | Node (Node (left1, v1, right1), v2, right2) -> 
      Node (deleteMin left1, v1, Node (right1, v2, right2))

end

