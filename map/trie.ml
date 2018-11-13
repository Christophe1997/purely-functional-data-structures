module type FiniteMap = sig
  type 'a t
  type key

  exception NotFound

  val empty : 'a t
  val bind : key -> 'a -> 'a t -> 'a t
  val lookup : key -> 'a t -> 'a
    
end

module Make (Map : FiniteMap) : FiniteMap = struct
  type key = Map.key list

  type 'a t = Trie of 'a option * 'a t Map.t

  exception NotFound

  let empty = Trie(None, Map.empty)

  let rec lookup key trie = match key, trie with
    | [], Trie (None, _) -> raise NotFound
    | [], Trie (Some x, _) -> x
    | k :: ks, Trie (_, m) -> lookup ks (Map.lookup k m)

  let rec bind key x trie = match key, x, trie with
    | [], x, Trie (_, m) -> Trie (Some x, m)
    | k :: ks, x, Trie (v, m) -> 
      let t = 
        try Map.lookup k m
        with NotFound -> empty in
      let t' = bind ks x t in
      Trie (v, Map.bind k t' m)

end

