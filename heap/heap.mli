type t
type elem

exception Empty

val empty : t
val isEmpty : t -> bool

val insert : elem -> t -> t
val merge : t -> t -> t

val findMin : t -> elem
val deleteMin : t -> t

