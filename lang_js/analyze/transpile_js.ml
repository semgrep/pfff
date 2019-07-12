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
open Common

module A = Ast_js
module C = Cst_js

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helpers for Ast_js_build.
 *
 * alt:
 *  - just call babel?
 *  - r2s's transpiler (calls babel internally?)
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
(* mostly a dupe of graph_code.ml, but avoid deps and take a tok *)
let cnt = ref 0
let gensym_name s tok =
  incr cnt;
  spf "!%s__%d!" s !cnt, tok

let compile_pattern (_expr, fname, fpname) varname pat =
  match pat with
  | C.PatObj x ->
    x |> C.unbrace |> C.uncomma |> List.map (fun pat ->
      match pat with
      (* { x, y } = varname; *)
      | C.PatId (name, None) ->
        let name = fname name in
        let init = A.ObjAccess (A.Id (varname, ref A.NotResolved),
                                A.PN name)
        in
        { A.v_name = name; v_kind = A.Let; v_init = init;
          v_resolved = ref A.NotResolved;
        }
      (* { x: y, z } = varname; *)
      | C.PatProp (pname, _tok, pat) ->
        let pname = fpname pname in
        (match pat with
        | C.PatId (name, None) ->
          let name = fname name in
          let init = A.ObjAccess (A.Id (varname, ref A.NotResolved),
                                  pname)
          in
          { A.v_name = name; v_kind = A.Let; v_init = init;
            v_resolved = ref A.NotResolved;
          }
        | _ -> failwith "TODO: pattern not handled"
        )
      | _ -> failwith "TODO: pattern not handled"
    )
  | _ -> failwith "TODO: pattern not handled"
        
   

let var_pattern (expr, fname, fpname) x =
  match x.C.vpat_init with 
  | None -> failwith "weird var_pattern without init; Part of ForOf?"
  | Some (tok, e) ->
    let e = expr e in
    let vname, vars = 
      match e with
      | A.Id (name, _) -> name, []
      | _ ->
        let intermediate = gensym_name "tmp" tok in
        let var = { A.v_name = intermediate; v_kind = A.Let; v_init = e;
                    v_resolved = ref A.NotResolved } in
        intermediate, [var]
    in
    vars @ compile_pattern (expr, fname, fpname) vname x.C.vpat