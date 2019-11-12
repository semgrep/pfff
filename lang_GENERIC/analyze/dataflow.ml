(* Iain Proctor, Yoann Padioleau, Jiao Li
 *
 * Copyright (C) 2009-2010 Facebook
 * Copyright (C) 2019 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

module F = Controlflow

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Dataflow analysis "framework".
 *
 * The goal of a dataflow analysis is to store information about each
 * variable at each program point, that is each node in a CFG
 * (e.g. whether a variable is "live" at a program point).
 * As you may want different kinds of information, the types below
 * are polymorphic. But each take as a key a variable name.
 *
 * todo:
 *  - could use a functor, so would not have all those 'a?
 *  - do we need other kind of information than variable environment?
 *    Dataflow analysis talks only about variables? for the belief analysis
 *    we actually want expressions instead.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* The comparison function uses only the name of a variable (a string), so
 * two variables at different positions in the code will be agglomerated
 * correctly in the Set or Map.
 *)
type var = string

(* convenient aliases *)
module VarMap = Map.Make(String)
module VarSet = Set.Make(String)

module NodeiSet = Set.Make(Int)

(* The final dataflow result; a map from each program point to a map containing
 * information for each variables.
 *
 * opti: this used to be a 'NodeiMap.t' instead of an 'array' but 'nodei'
 * are always int and array gives a 6x speedup according to Iain
 * so let's use array.
 *)
type 'a mapping = ('a inout) array

  (* the In and Out sets, as in Appel Modern Compiler in ML book *)
  and 'a inout = {
    in_env: 'a env;
    out_env: 'a env;
  }
    and 'a env = 'a VarMap.t

let empty_env () = VarMap.empty
let empty_inout () = {in_env = empty_env (); out_env = empty_env ()}

(*****************************************************************************)
(* Equality *)
(*****************************************************************************)

(* the environment is polymorphic, so we require to pass an eq for 'a *)
let eq_env eq e1 e2 =
  VarMap.equal eq e1 e2

let eq_inout eq io1 io2 =
  let eqe = eq_env eq in
  (eqe io1.in_env io2.in_env) && (eqe io1.out_env io2.out_env)

(*****************************************************************************)
(* Env manipulation *)
(*****************************************************************************)

(* useful helpers when the environment maps to a set of Nodes, e.g.,
 * for reaching definitions.
 *)

let (minus_env : NodeiSet.t env ->  NodeiSet.t env -> NodeiSet.t env) =
fun e1 e2 -> VarMap.fold (fun v s e' ->
  try
    let df = NodeiSet.diff (VarMap.find v e') s in
    if NodeiSet.is_empty df then VarMap.remove v e' else VarMap.add v df e'
  with Not_found -> e')
 e2 e1

let (add_env : NodeiSet.t env -> NodeiSet.t env -> NodeiSet.t env) =
fun e1 e2 -> VarMap.fold (fun v s e' ->
    let s2 = try NodeiSet.union s (VarMap.find v e') with Not_found -> s in
      VarMap.add v s2 e')
  e2 e1

(*****************************************************************************)
(* Debugging support *)
(*****************************************************************************)

let csv_append s v =
  if String.length s == 0 then v else s ^ "," ^ v

let array_fold_left_idx f = let idx = ref 0 in
  Array.fold_left (fun v e -> let r = f v !idx e in incr idx; r)


let ns_to_str ns =
  "{" ^
  NodeiSet.fold (fun n s -> csv_append s (string_of_int n)) ns "" ^
  "}"

let (env_to_str: ('a -> string) -> 'a env -> string) = fun val2str env ->
  VarMap.fold (fun dn v s -> s ^ dn ^ ":" ^ val2str v ^ " ") env ""

let (inout_to_str: ('a -> string) -> 'a inout -> string) = fun val2str inout ->
  spf "IN= %15s  OUT = %15s"
    (env_to_str val2str inout.in_env)
    (env_to_str val2str inout.out_env)

let mapping_to_str (fl: F.flow) val2str mapping =
  array_fold_left_idx (fun s ni v -> s ^
    (spf "%2d <- %7s: %15s %s\n"
      ni
      ((fl#predecessors ni)#fold (fun s (ni, _) ->
      csv_append s (string_of_int ni)) "")
     (F.short_string_of_node (fl#nodes#find ni))
     (inout_to_str val2str v)
  )) "" mapping

let (display_mapping: F.flow -> 'a mapping -> ('a -> string) -> unit) =
 fun flow mapping string_of_val ->
   pr (mapping_to_str flow string_of_val mapping)

(*****************************************************************************)
(* Main generic entry point *)
(*****************************************************************************)

(* The transition/transfer function. It is usually made from the
 * gens and kills.
 *
 * todo? having only a transfer function is enough ? do we need to pass
 * extra information to it ? maybe only the mapping is not enough. For
 * instance if in the code there is $x = &$g, a reference, then
 * we may want later to have access to this information. Maybe we
 * should pass an extra env argument ? Or maybe can encode this
 * sharing of reference in the 'a, so that when one update the
 * value associated to a var, its reference variable get also
 * the update.
 *)
type 'a transfn = 'a mapping -> F.nodei -> 'a inout

let rec fixpoint_worker eq mp trans flow succs work =
  if NodeiSet.is_empty work 
  then mp 
  else
    let ni = NodeiSet.choose work in
    let work' = NodeiSet.remove ni work in
    let old = mp.(ni) in
    let nu = trans mp ni in
    let work'' = 
         if eq_inout eq old nu
         then work'
         else begin
            (mp.(ni) <- nu; 
            NodeiSet.union work' (succs flow ni)) 
         end
     in
     fixpoint_worker eq mp trans flow succs work''


let forward_succs (f : F.flow) n = (f#successors n)#fold
  (fun s (ni, _) -> NodeiSet.add ni s) NodeiSet.empty

let backward_succs (f : F.flow) n = (f#predecessors n)#fold
  (fun s (ni, _) -> NodeiSet.add ni s) NodeiSet.empty

let (fixpoint:
    eq:('a -> 'a -> bool) ->
    init:'a mapping ->
    trans:'a transfn ->
    flow:F.flow ->
    forward: bool ->
   'a mapping) =
 fun ~eq ~init ~trans ~flow ~forward ->
  let succs = if forward then forward_succs else backward_succs in
  let work = 
    flow#nodes#fold (fun s (ni, _) -> NodeiSet.add ni s) NodeiSet.empty in
  fixpoint_worker eq init trans flow succs work
   

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let new_node_array (f: F.flow) v =
  let arr = Array.make f#nb_nodes v in
  (* sanity checking *)
  let len = Array.length arr in
  f#nodes#tolist +> List.iter (fun (ni, _nod) ->
    if ni >= len
    then failwith "the CFG nodei is bigger than the number of nodes"
  );
  arr
