(*
   Safe re-implementation of the unsafe parts of the standard List module.

   Several functions of OCaml's List module run in O(list length) stack
   space, occasionally resulting in crashes due to stack overflows.

   This module provides simple but safe implementations for those problematic
   functions.

   See also: suggestion for OCaml's standard library
             https://github.com/ocaml/ocaml/issues/10528

   At the time of writing, here's the popularity ranking of the List functions
   as used across pfff and semgrep-core:

    875 List.map *
    582 List.iter
    408 List.rev
    152 List.length
    109 List.hd
    108 List.fold_left
    101 List.filter
     79 List.flatten *
     78 List.mem
     62 List.assoc
     47 List.tl
     33 List.fold_right *
     26 List.exists
     24 List.for_all
     20 List.partition
     19 List.sort
     18 List.rev_map
     16 List.find
     15 List.nth
     12 List.concat * [alias for List.flatten]
      8 List.rev_append
      7 List.split
      6 List.append * [(@) is usually used instead]
      5 List.mem_assoc
      2 List.assoc_opt

   * = unsafe

   Other unsafe functions:
   - List.mapi
   - List.map2
   - List.fold_right2
   - List.remove_assoc
   - List.remove_assq
   - List.split
   - List.combine
   - List.merge
*)

val map : ('a -> 'b) -> 'a list -> 'b list
val flatten : 'a list list -> 'a list
val concat : 'a list list -> 'a list
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val append : 'a list -> 'a list -> 'a list

(* TODO
   val mapi
   val map2
   val fold_right2
   val remove_assoc
   val remove_assq
   val split
   val combine
   val merge
*)
