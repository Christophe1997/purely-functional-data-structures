type 'a t

val empty : 'a t
val isEmpty : 'a t -> bool

exception Empty

val cons : 'a -> 'a t -> 'a t
val head : 'a t -> 'a
val tail : 'a t -> 'a t

val lookup : int -> 'a t -> 'a
val update : int -> 'a -> 'a t -> 'a t

