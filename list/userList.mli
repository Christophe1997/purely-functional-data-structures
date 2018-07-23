(** userList.mli *)

type 'a t

val empty : 'a t
val isEmpty : 'a t -> bool

(** these exception does not matter *) 
exception Empty
exception Subscript 

val cons : 'a -> 'a t -> 'a t
val head : 'a t -> 'a
val tail : 'a t -> 'a t

(** list appled *)
val (++) : 'a t -> 'a t -> 'a t

(** update a node of the list *)
val update : 'a t -> int -> 'a -> 'a t
