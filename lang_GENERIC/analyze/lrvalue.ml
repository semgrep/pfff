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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helpers to extract the lvalues and rvalues of an expression 
 *
 * alternative:
 *  - have a proper lvalue type and an IL (a la CIL/PIL/RIL/...)
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

let rec visit_expr hook lhs expr =
  (* Used for known left hand value, e.g. reference *)
  let reclvl = visit_expr hook Lhs in
  let recl = visit_expr hook lhs in
  let recr = visit_expr hook Rhs in

  match expr with
  | Name (name, idinfo) ->
    (* calling the hook! *)
    hook lhs name idinfo

  | Assign(e, e1) ->
    recr e1;
    reclvl e;
  | AssignOp(e, _op, e1) ->
    (* x += b <=> x = x + b hence the call also to 'recr e' *)
    recr e1;
    recr e;
    reclvl e;

  (* otherwise regular recurse (could use a visitor) *)
  | L _ | Nop -> ()

  | IdSpecial _ -> ()
  (* todo: Special cases for function that are known to take implicit
   * lvalue, e.g., sscanf? 
   *)
  (* Todo: false positive because passsing by reference *)
  | Call (e, args) ->
    recr e;
    args |> List.iter (function
       | Arg e1 -> recr e1
       | ArgKwd _ | ArgType _ | ArgOther _ -> error_todo (E expr)
    );
  | ObjAccess(e, _id) ->
    recl e
  | ArrayAccess(e, e1) ->
    recr e1;
    recl e;
  | Conditional(e, e1, e2) ->
    recl e1;
    recl e2;
    recl e;
  | Cast(_, e) -> recr e

  (* TODO?? *)
  | Lambda _ | AnonClass _ -> ()
  | Yield e | Await e -> recr e

 | (Container (_, _)|Tuple _|Record _|Constructor (_, _)|Xml _|LetPattern (_, _)|MatchPattern (_, _)|
Seq _|Ref _|DeRef _|Ellipses _|OtherExpr (_, _)) -> 
  error_todo (E expr)

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
