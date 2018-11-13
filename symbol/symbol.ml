type symbol = string * int

let nextsym = ref 0

open Core


let table = Hashtbl.create (module String)

let make_symbol name = match Hashtbl.find table name with
  | Some i -> name, i
  | None ->
    let i = !nextsym in
    nextsym := i + 1;
    Hashtbl.set table ~key:name ~data:i;
    name, i

let name (s, n) = s
