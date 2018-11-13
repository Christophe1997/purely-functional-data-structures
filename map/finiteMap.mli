type 'a t
type key

exception NotFound

val empty : 'a t
val bind : key -> 'a -> 'a t -> 'a t
val lookup : key -> 'a t -> 'a (* raise NotFound if the key is not found *)

