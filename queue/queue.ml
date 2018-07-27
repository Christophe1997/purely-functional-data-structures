type 'a t = 'a list * 'a list

let empty = [], []

exception Empty

let isEmpty = function
  | [], _ -> true
  | _ -> false 

let head = function
  | [], _ -> raise Empty
  | x :: tl, _ -> x

let checkf = function
  | [], rear -> (List.rev rear, [])
  | queue -> queue

let tail = function
  | [], _ -> raise Empty
  | x :: tl, rear -> checkf (tl, rear)

let snoc x = function
  | (front, rear) -> checkf (front, x :: rear)

