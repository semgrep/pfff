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

open Ast_generic
module F = Controlflow

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Visitor of nodes and their expressions for dataflow purposes.
 *
 * This mostly factorize code having to deal with differences
 * between lvalue and rvalue.
 *
 * alt: 
 *  - alternate expression type where lvalue/rvalue are clearly separated?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* left or right handside of assignment (lvalue or rvalue) *)
type lhs_or_rhs =
  | Lhs
  | Rhs

(* function to apply when folding over nodes and expressions when
 * we encounter a variable. The 'a is usually a set, for example
 * for reaching definitions a NodeiSet.t, in which the function
 * will add or remove the nodei passed as a first parameter, depending
 * whether we are in an lvalue or rvalue context.
 *)
type 'a fold_fn = F.nodei -> Dataflow.var -> lhs_or_rhs -> 'a -> 'a

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let error_todo any = 
  let v = Meta_ast.vof_any any in
  let s = Ocaml.string_of_v v in
  pr2 s;
  failwith ("Dataflow_visitor:error_todo ")

(*****************************************************************************)
(* Expression Visitor *)
(*****************************************************************************)

let rec expr_fold ni fn lhs expr acc =
  (* Used for known left hand value, e.g. reference *)
  let reclvl = expr_fold ni fn Lhs in
  let recl = expr_fold ni fn lhs in
  let recr = expr_fold ni fn Rhs in

  match expr with
  | Name ((name, _tok), _idinfo) ->
    (* calling the hook! *)
    fn ni name lhs acc
  | Assign(e, e1) ->
    reclvl e (recr e1 acc)
  | AssignOp(e, _op, e1) ->
    (* x += b <=> x = x + b hence the call also to 'recr e' *)
    reclvl e (recr e (recr e1 acc))

  (* otherwise regular recurse (could use a visitor) *)
  | L _ | Nop -> acc

  | IdSpecial _ -> acc
  (* todo: Special cases for function that are known to take implicit
   * lvalue, e.g., sscanf? 
   *)
  (* Todo: false positive because passsing by reference *)
  | Call (e, args) ->
    recr e
    (List.fold_left
       (fun acc' -> function
       | Arg e1 -> recr e1 acc'
       | ArgKwd _ | ArgType _ | ArgOther _ -> error_todo (E expr)
       )
       acc args)
  | ObjAccess(e, _id) ->
    recl e acc
  | ArrayAccess(e, e1) ->
    recl e (recr e1 acc)
  | Conditional(e, e1, e2) ->
    recl e (recl e2 (recl e1 acc))
  | Cast(_, e) -> recr e acc

  | Lambda _ | AnonClass _ -> acc
  | Yield e | Await e -> recr e acc

 | (Container (_, _)|Tuple _|Record _|Constructor (_, _)|Xml _|LetPattern (_, _)|MatchPattern (_, _)|
Seq _|Ref _|DeRef _|Ellipses _|OtherExpr (_, _)) -> 
  error_todo (E expr)


(*****************************************************************************)
(* Node Visitor *)
(*****************************************************************************)

let (node_fold:  F.nodei -> 'a fold_fn -> Controlflow.node_kind -> 'a -> 'a) =
 fun ni fn node acc -> 
  match node with
  (* Nothing is needed if the node has no expr information*)
  | F.Enter | F.Exit
  | F.TrueNode | F.FalseNode
  | F.DoHeader | F.ForHeader
  | F.SwitchEnd | F.Case  | F.Default
  | F.TryHeader | F.CatchStart | F.Catch | F.TryEnd
  | F.Join
  | F.SimpleStmt (F.TodoSimpleStmt)
  | F.Continue None | F.Break None
      -> acc

  (* expr *)
  | F.IfHeader expr
  | F.WhileHeader expr
  | F.DoWhileTail expr
  | F.SwitchHeader expr
  | F.Throw expr
  | F.SimpleStmt (F.ExprStmt (expr))
  | F.Return (expr)
  | F.Continue (Some expr) | F.Break (Some expr)
      -> expr_fold ni fn Rhs expr acc
  | F.Parameter (_) ->
    (* env.fold_fn name true acc *)
    raise Todo
  | F.ForeachHeader (*var_list *) ->
        raise Todo
(*
        List.fold_left (fun acc' var ->
          match var with
          | (Some _, _) -> raise Todo
          | (None, IdVar(DName(s, _), _)) ->
            env.fold_fn s true acc'
          | _ -> acc') acc var_list
*)

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let (flow_fold: 'a fold_fn -> 'a -> F.flow -> 'a) =
 fun fold_fn acc flow -> 
  flow#nodes#fold
   (fun acc' (ni, nd) -> node_fold ni fold_fn nd.F.n acc') acc

let flow_fold_lv fold_fn acc flow = 
  flow_fold
    (fun ndi var lhs acc' -> 
        if lhs = Lhs
        then fold_fn ndi var acc' 
        else acc'
     ) acc  flow

let flow_fold_rv fold_fn acc flow = 
  flow_fold
    (fun ndi var lhs acc' -> 
       if lhs = Rhs
       then fold_fn ndi var acc' 
       else acc'
    ) acc flow
