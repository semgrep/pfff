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

open Dataflow

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Example of dataflow analysis: reaching definitions *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* For a reaching definitions analysis, the dataflow result is
 * a map from each program point (as usual), to a map from each
 * variable (as usual), to a set of nodes that define this variable
 * that are visible at this program point.
 *
 * For instance on:
 *
 * 1: $a = 1;
 * 2: if(...) {
 * 3:   $a = 2;
 * 4: } else {
 * 5:   $a = 3;
 * 6: }
 * 7: echo $a;
 *
 * then at the program point (node index) 7, then for $a the nodei set
 * is {3, 5}, but not '1'.
 *)
type reaching_mapping = NodeiSet.t mapping

let add_def d nm ni =
  let v =
    try NodeiSet.add ni (VarMap.find nm d)
    with Not_found -> NodeiSet.singleton ni
  in
  VarMap.add nm v d

let (vars: F.flow -> VarSet.t) =
  Dataflow_visitor.flow_fold (fun _ va _ vs -> VarSet.add va vs) VarSet.empty

let (reaching_defs: VarSet.t -> F.flow -> NodeiSet.t env) = fun _vars ->
  Dataflow_visitor.flow_fold_lv (fun ni va acc -> add_def acc va ni) VarMap.empty

let up_map k f mp = mp.(k) <- f mp.(k); mp

let up_set_map k v mp = up_map k (VarSet.add v) mp

(* gen/kill *)
let (reaching_gens: VarSet.t -> F.flow -> VarSet.t array) = 
 fun _vars fl ->
  Dataflow_visitor.flow_fold_lv (fun ni v gs -> up_set_map ni v gs)
    (new_node_array fl VarSet.empty) fl

let (reaching_kills:
   NodeiSet.t env -> VarSet.t -> F.flow -> (NodeiSet.t env) array) =
 fun ds _vars fl -> 
  Dataflow_visitor.flow_fold_lv (fun ni va ks ->
    let d = NodeiSet.remove ni (VarMap.find va ds) in
    up_map ni (fun v -> VarMap.add va d v) ks)
      (new_node_array fl (empty_env())) fl

(*
 * This algorithm is taken from Modern Compiler Implementation in ML, Appel,
 * 1998, pp. 382.
 *
 * The transfer is setting in'[n] = U_{p in pred[n]} out[p] and
 * out'[n] = gen[n]U(in[n] - kill[n]) where gen[n] is {n} if there in a
 * definition at n and {} otherwise, and kill[n] is the set of all definitions
 * of the variable being defined at n except the one at n.
 *)
let (reaching_transfer:
   gen:VarSet.t array ->
   kill:(NodeiSet.t env) array ->
   flow:F.flow ->
   NodeiSet.t transfn) =
 fun ~gen ~kill ~flow ->
  fun mp ni ->

  let new_in = (flow#predecessors ni)#fold (fun s (nip, _) ->
      add_env s mp.(nip).out_env) VarMap.empty in
  let in_k = minus_env new_in kill.(ni) in
  let new_out = VarSet.fold (fun v e -> VarMap.add v
      (try NodeiSet.add ni (VarMap.find v e)
       with Not_found -> NodeiSet.singleton ni) e)
    gen.(ni) in_k in
  {in_env = new_in; out_env = new_out}

let (reaching_fixpoint: F.flow -> reaching_mapping) = fun flow ->
  let vars = vars flow in
  let defs = reaching_defs vars flow in
  let gen = reaching_gens vars flow in
  let kill = reaching_kills defs vars flow in

  fixpoint
    ~eq:NodeiSet.equal
    ~init:(new_node_array flow (empty_inout ()))
    ~trans:(reaching_transfer ~gen ~kill ~flow)
    ~forward:true
    ~flow

(*****************************************************************************)
(* Dataflow pretty printing *)
(*****************************************************************************)



let display_reaching_dflow flow mp =
  let string_of_ni ni =
    let node = flow#nodes#assoc ni in
    match node.F.i with
    | None -> "Unknown location"
    | Some(info) ->
      let info = Parse_info.token_location_of_info info in
      spf "%s:%d:%d: "
        info.Parse_info.file info.Parse_info.line info.Parse_info.column
  in
  let arr = Array.make flow#nb_nodes true in
  (* Set the flag to false if the node has defined anything *)
  let flow_lv_fn = fun ni _name arr ->
    let node = flow#nodes#assoc ni in
    (match node.F.n with
    | F.SimpleStmt (F.ExprStmt _) ->
      arr.(ni) <- false
    | _ -> ());
    arr
  in
  let arr = Dataflow_visitor.flow_fold_lv flow_lv_fn arr flow in

  (* Now flag the def if it is ever used on rhs *)
  (* Node id -> var name -> acc -> acc' *)
  let flow_rv_fn = fun ni name arr ->
    let in_env = mp.(ni).in_env in
    (try
       let ns = VarMap.find name in_env in
       NodeiSet.fold (fun n _ -> arr.(n) <- true) ns ()
     with
     | Not_found -> pr (spf "%s: Undefined variable" (string_of_ni ni)));
    arr
  in
  let arr = Dataflow_visitor.flow_fold_rv flow_rv_fn arr flow in
  let i = ref 0 in
  List.iter (fun x ->
    if (not x)
    then pr (spf "%s: Dead Assignment" (string_of_ni !i));
    incr i
  ) (Array.to_list arr)
