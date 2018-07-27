type 'a t = 'a digit list
and 'a digit = Zero | One of 'a tree
and 'a tree = Leaf of 'a | Node of int * 'a tree * 'a tree
                                     
exception Empty

let empty = []
let isEmpty = function 
  | [] -> true
  | _ -> false

let size = function
  | Leaf _ -> 1
  | Node (n, _, _) -> n

let link t1 t2 = Node (size t1 + size t2, t1, t2)

let cons x ts = 
  let rec consTree t = function
    | [] -> [One t]
    | Zero :: tl -> One t :: tl
    | One t1 :: tl -> Zero :: consTree (link t t1) tl
  in
  consTree (Leaf x) ts

let rec unconsTree = function
  | [] -> raise Empty
  | [One t] -> t, []
  | One t :: tl -> t, Zero :: tl
  | Zero :: tl -> match (unconsTree tl) with
    | Node (_, t1, t2), ts -> t1, One t2 :: ts
    | _ -> assert false

let head ts = match (unconsTree ts) with
  | Leaf x, _ -> x
  | _ -> assert false

let tail ts = match (unconsTree ts) with
  | _, tl -> tl

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
  | Zero :: tl -> lookup i tl
  | One t :: tl -> 
    if i < size t 
    then lookupTree i t
    else lookup (i - size t) tl

let rec update i x = function
  | [] -> raise Empty
  | Zero :: tl -> Zero :: update i x tl
  | One t :: tl -> 
    if i < size t
    then One (updateTree i x t) :: tl
    else One t :: update (i - size t) x tl

