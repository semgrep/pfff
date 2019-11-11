module F = Controlflow

module VarMap : Map.S with type key = String.t
module VarSet : Set.S with type elt = String.t
module NodeiSet : Set.S with type elt = Int.t
type nodei = Ograph_extended.nodei

type 'a mapping = 'a inout array
  and 'a inout = { 
    in_env : 'a env; 
    out_env : 'a env; 
   }
  and 'a env = 'a VarMap.t

val empty_env : unit -> 'a VarMap.t
val empty_inout : unit -> 'a inout

type 'a transfn = 'a mapping -> nodei -> 'a inout

(* main entry point *)
val fixpoint :
  eq:('a -> 'a -> bool) ->
  init:'a mapping ->
  trans:'a transfn -> 
  flow:F.flow -> 
  forward:bool -> 
  'a mapping

(* helpers *)

val eq_env : ('a -> 'a -> bool) -> 'a VarMap.t -> 'a VarMap.t -> bool
val eq_inout : ('a -> 'a -> bool) -> 'a inout -> 'a inout -> bool
val eq_mapping : ('a -> 'a -> bool) -> 'a inout array -> 'a inout array -> bool

val minus_env : NodeiSet.t env -> NodeiSet.t env -> NodeiSet.t env
val add_env   : NodeiSet.t env -> NodeiSet.t env -> NodeiSet.t env

val fixpoint_worker :
  ('a -> 'a -> bool) ->
  'a inout array ->
  ('a inout array -> NodeiSet.elt -> 'a inout) ->
  'b -> ('b -> NodeiSet.elt -> NodeiSet.t) -> NodeiSet.t -> 'a inout array

val forward_succs : F.flow -> Ograph_extended.nodei -> NodeiSet.t
val backward_succs : F.flow -> Ograph_extended.nodei -> NodeiSet.t


val csv_append : string -> string -> string
val array_fold_left_idx : ('a -> int -> 'b -> 'a) -> 'a -> 'b array -> 'a
val ns_to_str : NodeiSet.t -> string
val env_to_str : ('a -> string) -> 'a env -> string
val inout_to_str : ('a -> string) -> 'a inout -> string
val mapping_to_str : F.flow -> ('a -> string) -> 'a inout array -> string
