(* Yoann Padioleau
 *
 * Copyright (C) 2019 Yoann Padioleau
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

module A = Ast_js
module C = Cst_js

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helpers for Ast_js_build.
 *
 * alt:
 *  - just call babel?
 *
 * related:
 *  - babel and its "polyfill"
 *)

(*****************************************************************************)
(* Xhp *)
(*****************************************************************************)
let id_of_tag tag =
  A.Id (tag, ref A.NotResolved)

let xhp_attr_value expr x =
  match x with
  | C.XhpAttrString str -> A.String str
  | C.XhpAttrExpr (_, e, _) -> expr e

(* todo: should probably use Obj instead of tuples with string keys *)
let xhp_attribute expr x = 
  match x with
  | C.XhpAttrNoValue (str) -> A.Arr [A.String str]
  | C.XhpAttrValue (str, _tok, attrval) ->
    let v = xhp_attr_value expr attrval in
    A.Arr [A.String str; v]
  | C.XhpAttrSpread (_, (tokdot, e), _) ->
    A.Apply (A.IdSpecial (A.Spread, tokdot), [expr e])

let rec xhp expr x =
  match x with
  | C.XhpSingleton (tag, attrs, _tok) ->
    let id = id_of_tag tag in
    let args1 = List.map (xhp_attribute expr) attrs in
    let args2 = [] in
    (* TODO: is it the actual result? good enough for codegraph for now *)
    A.Apply(id, [A.Arr args1; A.Arr args2])
  | C.Xhp (tag, attrs, _tok, body, _endtag_opt) ->
    let id = id_of_tag tag in
    let args1 = List.map (xhp_attribute expr) attrs in
    let args2 = List.map (xhp_body expr) body in
    A.Apply (id, [A.Arr args1; A.Arr args2])
and xhp_body expr x = 
  match x with
  (* todo: contain enclosing quote? *)
  | C.XhpText str -> A.String str
  | C.XhpNested x -> xhp expr x
  | C.XhpExpr (_, eopt, _) -> 
     (match eopt with
     | None -> A.Nop
     | Some e -> expr e
     )

(*****************************************************************************)
(* Patterns *)
(*****************************************************************************)
