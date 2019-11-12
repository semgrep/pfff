module F = Controlflow

type var = string
module VarMap : Map.S with type key = String.t
module VarSet : Set.S with type elt = String.t

(* types *)
type 'a mapping = 'a inout array
  and 'a inout = { 
    in_env : 'a env; 
    out_env : 'a env; 
   }
  and 'a env = 'a VarMap.t

val empty_env : unit -> 'a VarMap.t
val empty_inout : unit -> 'a inout

(* useful 'a for mapping: a set of nodes (via their indices) *)
module NodeiSet : Set.S with type elt = Int.t


type 'a transfn = 'a mapping -> F.nodei -> 'a inout

(* main entry point *)
val fixpoint :
  eq:('a -> 'a -> bool) ->
  init:'a mapping ->
  trans:'a transfn -> 
  flow:F.flow -> 
  forward:bool -> 
  'a mapping

(* helpers *)
val minus_env : NodeiSet.t env -> NodeiSet.t env -> NodeiSet.t env
val add_env   : NodeiSet.t env -> NodeiSet.t env -> NodeiSet.t env

val new_node_array: F.flow -> 'a -> 'a array

val ns_to_str : NodeiSet.t -> string

val display_mapping :
  F.flow -> 'a mapping -> ('a -> string) -> unit
