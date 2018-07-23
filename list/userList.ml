type 'a t = 
  | Nil 
  | Cons of 'a * ('a t) 

let empty = Nil

exception Empty
exception Subscript

let isEmpty = function 
  | Nil -> true
  | _ -> false

let cons(hd, tl) = Cons (hd, tl)

let head = function
  | Nil -> raise Empty
  | Cons (hd, tl) -> hd

let tail = function
  | Nil -> raise Empty
  | Cons (hd, tl) -> tl

let rec (++) xs ys = match xs with
  | Nil -> ys
  | Cons (hd, tl) -> tl ++ Cons (hd, ys)

let rec update xs idx x = 
  let rec updateK xs idx x k = match xs, idx with
    | Nil, _ -> raise Subscript
    | Cons (hd, tl), 0 -> k (Cons (x, tl))
    | Cons (hd, tl), (_ as n) -> updateK tl (n - 1) x (fun var1 -> Cons (x, var1))
  in
  updateK xs idx x (fun var1 -> var1)
