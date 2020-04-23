type nodei = int

type var = string
module VarMap : Map.S with type key = String.t
module VarSet : Set.S with type elt = String.t

(* return value of a dataflow analysis *)
type 'a mapping = 'a inout array
  and 'a inout = { 
    in_env : 'a env; 
    out_env : 'a env; 
   }
  and 'a env = 'a VarMap.t

val empty_env : unit -> 'a VarMap.t
val empty_inout : unit -> 'a inout

type 'a transfn = 'a mapping -> nodei -> 'a inout

val varmap_union: 
  ('a -> 'a -> 'a) -> 
  'a env -> 'a env -> 'a env
val varmap_diff: 
  ('a -> 'a -> 'a) -> ('a -> bool) -> 
  'a env -> 'a env -> 'a env


(* useful 'a for mapping: a set of nodes (via their indices) *)
module NodeiSet : Set.S with type elt = Int.t
(* helpers *)
val union_env : NodeiSet.t env -> NodeiSet.t env -> NodeiSet.t env
val diff_env  : NodeiSet.t env -> NodeiSet.t env -> NodeiSet.t env

val add_var_and_nodei_to_env: 
  var -> nodei -> NodeiSet.t env -> NodeiSet.t env
val add_vars_and_nodei_to_env: 
  VarSet.t -> nodei -> NodeiSet.t env -> NodeiSet.t env

val ns_to_str : NodeiSet.t -> string


module type Flow = sig
  type node
  type edge
  type flow = (node, edge) Ograph_extended.ograph_mutable
  val short_string_of_node: node -> string
end

module Make (F: Flow) : sig

(* main entry point *)
val fixpoint :
  eq:('a -> 'a -> bool) ->
  init:'a mapping ->
  trans:'a transfn -> 
  flow:F.flow -> 
  forward:bool -> 
  'a mapping

val new_node_array: F.flow -> 'a -> 'a array

(* debugging output *)
val display_mapping :
  F.flow -> 'a mapping -> ('a -> string) -> unit
end
