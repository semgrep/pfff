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
module V = Visitor_ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helpers to extract the lvalues and rvalues of an expression.
 *
 * alternatives:
 *  - have a proper lvalue type and an IL (a la CIL/RIL/PIL/...)
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* left or right handside of assignment (lvalue or rvalue) context *)
type lhs_or_rhs =
  | Lhs
  | Rhs

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error_todo any = 
  let v = Meta_ast.vof_any any in
  let s = Ocaml.string_of_v v in
  pr2 s;
  failwith ("Dataflow_visitor:error_todo ")

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

(* Recursively visit the expression.
 * alt: 
 *  - use a visitor? and then do things differently only when inside an
 *    Assign?
 *)
let rec visit_expr hook lhs expr =
      
  (* recurse lvalue (used for known left hand value, e.g. in left assign *)
  let reclvl = visit_expr hook Lhs in
  (* recurse left (possible lvalue) *)
  let recl = visit_expr hook lhs in
  (* recurse right (rvalue context) *)
  let recr = visit_expr hook Rhs in

  let anyhook hook lhs any =
    let v = V.mk_visitor { V.default_visitor with 
      V.kexpr = (fun (_k, _anyf) e -> 
        visit_expr hook lhs e
        (* do not call k here! *)
      )
     (* todo? should no go through FuncDef? intercept kdef? 
      * TODO: should also consider PatVar?
      *  with PatVar we will miss some lvalue, but it will just lead
      *  to some FNs for liveness, not FPs.
      *)
    } in
    v any
  in

  match expr with
  (* the leaf *)

  | Name ((name, _name_info), idinfo) ->
    (* calling the hook! *)
    hook lhs name idinfo

  (* the assignements *)

  | Assign(e, _tok, e1) ->
    (* definitely in a Rhs context *)
    recr e1;
    (* definitely in a Lhs context *)
    reclvl e;

  | AssignOp(e, _op, e1) ->
    recr e1;
    (* x += b <=> x = x + b hence the call also to 'recr e' *)
    recr e;
    reclvl e;


  (* possible lvalues (also rvalues, hence the call to recl, not reclvl) *)

  | Tuple xs -> xs |> List.iter recl

  | Container (typ, xs) ->
    (match typ with
    (* used on lhs? *)
    | Array | List -> xs |> List.iter recl
    (* never used on lhs *)
    | Set | Dict -> xs |> List.iter recr
    )   

  (* composite lvalues that are actually not themselves lvalues *)

  | ObjAccess(e, _id) ->
    (* bugfix: this is not recl here! in 'x.fld = 2', x itself is not
     * an lvalue; 'x.fld' is *)
    recr e 
  | ArrayAccess(e, e1) ->
    recr e1;
    recr e;
  | SliceAccess (e, e1, e2, e3) ->
      [e1;e2;e3] |> List.map Ast_generic.opt_to_nop |> List.iter recr;
      recr e

  | DeRef e -> recr e
  | Ref e -> recr e 

  (* otherwise regular recurse (could use a visitor) *)

  | L _ | Nop -> ()

  | IdSpecial _ -> ()
  (* todo: Special cases for function that are known to take implicit
   * lvalue, e.g., sscanf? 
   *)

  (* todo? some languages allow function return value to be an lvalue? *)
  | Call (e, args) ->
    recr e;
    args |> List.iter (function
       (* Todo: false positive because passsing by reference? *)
       | Arg e -> recr e
       | ArgKwd (_id, e) -> recr e
       | ArgType _ -> ()
       | ArgOther (_, anys) -> List.iter (anyhook hook Rhs) anys
    );

  | Cast(_t, e) -> recr e

  (* Do some languages allow this to be part of an assign? *)
  | Conditional(e, e1, e2) ->
    recr e1;
    recr e2;
    recr e;

  (* TODO: we should also process the lambda and check for useless
   * assignements in it
   *)
  | Lambda def -> 
    (* Is it enough to just call anyhook and return everything as in:
     *
     *    anyhook hook Rhs (S def.fbody)
     *
     * No, because the body may introduce some assigns to its parameter 
     * or its own locals, and then those locals will be returned as lvalues,
     * which will then lead to some useless_assign because the
     * enclosing functions will probably not use the same local var.
     *
     * As a first step, we could just filter and only return rvalues
     * TODO As a second step we could return lvalues but only for variables
     * tagged as an EnclosedVar.
     *)
      let filter_rvalue_hook lhs name idinfo =
        if lhs = Rhs then hook lhs name idinfo
      in
      anyhook filter_rvalue_hook Rhs (S def.fbody)


  | AnonClass _ -> ()

  | Yield (e, _is_yield_from) -> recr e
  | Await e -> recr e

  | Record xs -> 
     xs |> List.iter (fun field ->
       anyhook hook Rhs (Fld field)
     )

  | Constructor (_name, es) -> List.iter recr es
  | Xml anys -> List.iter (anyhook hook Rhs) anys

  | LetPattern (pat, e) ->
      anyhook hook Lhs (P pat);
      recr e
  | MatchPattern (_, _)
     -> error_todo (E expr)

  | Seq xs -> List.iter recr xs

  | Ellipsis _tok -> ()

  | OtherExpr (_other_xxx, anys) -> List.iter (anyhook hook Rhs) anys


(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let lvalues_of_expr expr = 
  let context = Rhs in
  let acc = ref [] in
  visit_expr (fun lhs name idinfo ->
    if lhs = Lhs
    then Common.push (name, idinfo) acc
  ) context expr;
  List.rev !acc

let rvalues_of_expr expr =
  let context = Rhs in
  let acc = ref [] in
  visit_expr (fun lhs name idinfo ->
    if lhs = Rhs
    then Common.push (name, idinfo) acc
  ) context expr;
  List.rev !acc
