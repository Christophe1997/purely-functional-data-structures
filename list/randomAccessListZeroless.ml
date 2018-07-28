type 'a t = 'a digit list
and 'a digit = One of 'a tree | Two of 'a tree * 'a tree
and 'a tree = Leaf of 'a | Node of int * 'a tree * 'a tree

let empty = []
let isEmpty = function
  | [] -> true
  | _ -> false

exception Empty

let size = function
  | Leaf _ -> 1
  | Node (n, _, _) -> n

let link t1 t2 = Node (size t1 + size t2, t1, t2)

let cons x ts = 
  let rec consTree t = function
    | [] -> [One t]
    | One t1 :: tl -> Two (t, t1) :: tl
    | Two (t1, t2) :: tl -> One t :: consTree (link t1 t2) tl
  in consTree (Leaf x) ts

let head = function
  | One Leaf x :: tl -> x
  | Two (Leaf x, _) :: tl -> x
  | _ -> assert false

let tail = function
  | [] -> raise Empty
  | [One _] -> []
  | One _ :: One Node (_, left, right) :: tl -> Two (left, right) :: tl
  | One _ :: Two (t, Node (_, left, right)) :: tl -> Two (left, right) :: One t :: tl
  | Two (_, t) :: tl -> One t :: tl
  | _ -> assert false

let rec lookupTree i t = match i, t with
  | 0, Leaf x -> x
  | i, Leaf _ -> raise Empty
  | i, Node (n, left, right) ->
    if i < n / 2
    then lookupTree i left
    else lookupTree (i - n / 2) right

let rec updateTree i x t =match i, t with
  | 0, Leaf _ -> Leaf x
  | i, Leaf _ -> raise Empty
  | i, Node (n, left, right) -> 
    if i < n / 2
    then Node (n, updateTree i x left, right)
    else Node (n, left, updateTree (i - n / 2) x right)

let rec lookup i = function
  | [] -> raise Empty
  | One t :: tl ->
    if i < size t
    then lookupTree i t
    else lookup (i - size t) tl
  | Two (t1, t2) :: tl ->
    if i < size t1 
    then lookupTree i t1
    else if i < size t2
    then lookupTree (i - size t1) t2
    else lookup (i - size t1 - size t2) tl

let rec update i x = function
  | [] -> raise Empty
  | (One t as hd) :: tl ->
    if i < size t 
    then One (updateTree i x t) :: tl
    else hd :: update (i - size t) x tl
  | (Two (t1, t2) as hd) :: tl ->
    if i < size t1
    then Two (updateTree i x t1, t2) :: tl
    else if i < size t2
    then Two (t1, updateTree (i - size t1) x t2) :: tl
    else hd :: update (i - size t1 - size t2) x tl
           
