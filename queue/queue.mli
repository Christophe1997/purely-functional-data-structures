type 'a t

val empty : 'a t

val isEmpty : 'a t -> bool

(** snoc means "cons on the right" *)
val snoc : 'a -> 'a t -> 'a t

val head : 'a t -> 'a

val tail : 'a t -> 'a t
         
