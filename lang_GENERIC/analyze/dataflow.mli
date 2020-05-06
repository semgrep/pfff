(*s: pfff/lang_GENERIC/analyze/dataflow.mli *)
(*s: type [[Dataflow.nodei]] *)
type nodei = int
(*e: type [[Dataflow.nodei]] *)

(*s: type [[Dataflow.var]] *)
type var = string
(*e: type [[Dataflow.var]] *)
(*s: module [[Dataflow.VarMap]] *)
module VarMap : Map.S with type key = String.t
(*e: module [[Dataflow.VarMap]] *)
(*s: module [[Dataflow.VarSet]] *)
module VarSet : Set.S with type elt = String.t
(*e: module [[Dataflow.VarSet]] *)

(* Return value of a dataflow analysis.
 * The array is indexed by nodei.
 *)
(*s: type [[Dataflow.mapping]] *)
type 'a mapping = 'a inout array
(*e: type [[Dataflow.mapping]] *)
(*s: type [[Dataflow.inout]] *)
  and 'a inout = { 
    in_env : 'a env; 
    out_env : 'a env; 
   }
(*e: type [[Dataflow.inout]] *)
(*s: type [[Dataflow.env]] *)
  and 'a env = 'a VarMap.t
(*e: type [[Dataflow.env]] *)

(*s: signature [[Dataflow.empty_env]] *)
val empty_env : unit -> 'a VarMap.t
(*e: signature [[Dataflow.empty_env]] *)
(*s: signature [[Dataflow.empty_inout]] *)
val empty_inout : unit -> 'a inout
(*e: signature [[Dataflow.empty_inout]] *)

(*s: type [[Dataflow.transfn]] *)
type 'a transfn = 'a mapping -> nodei -> 'a inout
(*e: type [[Dataflow.transfn]] *)

(*s: signature [[Dataflow.varmap_union]] *)
val varmap_union: 
  ('a -> 'a -> 'a) -> 
  'a env -> 'a env -> 'a env
(*e: signature [[Dataflow.varmap_union]] *)
(*s: signature [[Dataflow.varmap_diff]] *)
val varmap_diff: 
  ('a -> 'a -> 'a) -> ('a -> bool) -> 
  'a env -> 'a env -> 'a env
(*e: signature [[Dataflow.varmap_diff]] *)


(* common/useful 'a for mapping: a set of nodes (via their indices),
 * used for example in the reaching analysis.
 *)
(*s: module [[Dataflow.NodeiSet]] *)
module NodeiSet : Set.S with type elt = Int.t
(*e: module [[Dataflow.NodeiSet]] *)
(*s: signature [[Dataflow.union_env]] *)
(* helpers *)
val union_env : NodeiSet.t env -> NodeiSet.t env -> NodeiSet.t env
(*e: signature [[Dataflow.union_env]] *)
(*s: signature [[Dataflow.diff_env]] *)
val diff_env  : NodeiSet.t env -> NodeiSet.t env -> NodeiSet.t env
(*e: signature [[Dataflow.diff_env]] *)

(*s: signature [[Dataflow.add_var_and_nodei_to_env]] *)
val add_var_and_nodei_to_env: 
  var -> nodei -> NodeiSet.t env -> NodeiSet.t env
(*e: signature [[Dataflow.add_var_and_nodei_to_env]] *)
(*s: signature [[Dataflow.add_vars_and_nodei_to_env]] *)
val add_vars_and_nodei_to_env: 
  VarSet.t -> nodei -> NodeiSet.t env -> NodeiSet.t env
(*e: signature [[Dataflow.add_vars_and_nodei_to_env]] *)

(*s: signature [[Dataflow.ns_to_str]] *)
val ns_to_str : NodeiSet.t -> string
(*e: signature [[Dataflow.ns_to_str]] *)

(* we use now a functor so we can reuse the same code for dataflow on
 * the IL (Il.cfg) or generic AST (Controlflow.flow)
 *)
(*s: module type [[Dataflow.Flow]] *)
module type Flow = sig
  type node
  type edge
  type flow = (node, edge) Ograph_extended.ograph_mutable
  val short_string_of_node: node -> string
end
(*e: module type [[Dataflow.Flow]] *)
(*s: functor signature [[Dataflow.Make]] *)
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
(*e: functor signature [[Dataflow.Make]] *)
(*e: pfff/lang_GENERIC/analyze/dataflow.mli *)
