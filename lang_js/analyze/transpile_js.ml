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
(* Poor's man Javascript transpiler! 
 * Probably incorrect and incomplete but good enough for codegraph.
 *
 * You can test the babel transpiler and see live how it transpiles code
 * here: https://babeljs.io/repl (great ressource).
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
(* TODO probably incomplete *)

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
(* TODO incomplete, handle all patterns, and compare with
 * what babel actually does.
 *)

(* mostly a dupe of graph_code.ml, but avoid deps and take a tok *)
let cnt = ref 0
let gensym_name s tok =
  incr cnt;
  spf "!%s__%d!" s !cnt, tok


let var_of_simple_pattern (fname) init_builder pat =
  match pat with
  (* { x, y } = varname; *)
  | C.PatId (name, None) ->
    let name = fname name in
    let init = init_builder name in
    { A.v_name = name; v_kind = A.Let; v_init = init;
      v_resolved = ref A.NotResolved;
    }
  | _ -> failwith "TODO: simple pattern not handled"


let compile_pattern (_expr, fname, fpname) varname pat =
  match pat with
  (* 'var { x, y } = varname'  -~> 'var x = varname.x; var y = varname.y;' *)
  | C.PatObj x ->
    x |> C.unbrace |> C.uncomma |> List.map (fun pat ->
     (match pat with
     | C.PatId _ ->
       let init_builder name = 
         A.ObjAccess (A.Id (varname, ref A.NotResolved), A.PN name)
       in
       var_of_simple_pattern (fname) init_builder pat 
     (* { x: y, z } = varname; *)
     | C.PatProp (pname, _tok, pat) ->
       let pname = fpname pname in
       (match pat with
       | C.PatId (name, None) ->
         let name = fname name in
         let init = 
           A.ObjAccess (A.Id (varname, ref A.NotResolved), pname) in
         { A.v_name = name; v_kind = A.Let; v_init = init;
           v_resolved = ref A.NotResolved;
         }
       | _ -> failwith "TODO: PatObj pattern not handled"
       )
     | _ -> failwith "TODO: PatObj pattern not handled"
    ))
  (* 'var [x,y] = varname' -~> 'var x = varname[0]; var y = varname[1] *)
  | C.PatArr x ->
    let xs = x |> C.unbrace in
    let idx = ref 0 in
    let aux_pat pat =
      match pat with
      | C.PatId _ ->
        let init_builder (_name, tok) = 
          A.ArrAccess (A.Id (varname, ref A.NotResolved), 
                       A.Num (string_of_int !idx, tok))
        in
        var_of_simple_pattern (fname) init_builder pat
      | C.PatDots (tok, pat) -> 
         let init_builder (_name, _tok) = 
          A.Apply(A.ObjAccess (A.Id (varname, ref A.NotResolved),
                              (A.PN (("slice", tok)))),
                  [A.Num (string_of_int !idx, tok)])
        in
        var_of_simple_pattern (fname) init_builder pat
      | _ -> failwith "TODO: PatArr pattern not handled"
    in
    let rec aux xs = 
      match xs with
      | [] -> []
      | [Right _] -> failwith "useless comma"
      | [Left pat] -> [aux_pat pat]
      | (Left pat)::(Right _)::xs -> 
           let var = aux_pat pat in
           incr idx;
           var :: aux xs
      (* elision *)
      | (Right _)::xs -> 
           incr idx;
           aux xs
      | Left _::Left _::_ -> failwith "Impossible Left Left"
    in
    aux xs
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

(*****************************************************************************)
(* Iterator for of *)
(*****************************************************************************)
(* for (xx of yy) -~> 
 *   for (var _iterator = yy[Symbol.iterator](), _step;
 *        !(_step = _iterator.next()).done;;) {
 *     xx = _step.value;
 * TODO probably incomplete.
 *)

let forof (lhs_var, tok, e2, st) (expr, stmt, var_binding) =
  let e2 = expr e2 in
  let st = stmt st in

  let iterator = "!iterator!", tok in
  let step = "!step!", tok in
  let symbol_iterator = 
    A.ObjAccess (A.Id (("Symbol", tok), ref A.NotResolved),
                 A.PN ("iterator", tok))
  in

  let for_init = 
    Left [
      { A.v_name = iterator; v_kind = A.Let; v_resolved = ref A.NotResolved;
        v_init = A.Apply (A.ArrAccess (e2, symbol_iterator), []) };
      { A.v_name = step; v_kind = A.Let; v_resolved = ref A.NotResolved;
        v_init = A.Nop; }
    ]
  in
  let for_cond = 
    A.Apply (A.IdSpecial (A.Not, tok), [
      A.ObjAccess (A.Assign (A.Id (step, ref A.NotResolved),
                          A.Apply (A.ObjAccess (A.Id (iterator, 
                                                      ref A.NotResolved),
                                                A.PN ("next", tok)),
                                   [])),
        A.PN ("done", tok))
       ])
  in
  let step_value = A.ObjAccess (A.Id (step, ref A.NotResolved),
                               A.PN ("value", tok)) 
  in
  let step_value_cst = 
    C.Period (C.V step, tok, ("value", tok)) 
  in

  let vars_or_assign_stmts =
   match lhs_var with
   | C.LHS2 e -> 
     let e = expr e in
     [A.ExprStmt (A.Assign (e, step_value))]
   | C.ForVar ((vkind,_tok), binding) -> 
      let binding = 
        match binding with
        | C.VarClassic x -> 
            if x.C.v_init <> None
            then failwith "for-of loop variable can not have an initializer";
            C.VarClassic { x with C.v_init = Some (tok, step_value_cst) }
        | C.VarPattern x ->
            if x.C.vpat_init <> None
            then failwith "for-of loop variable can not have an initializer";
            C.VarPattern { x with C.vpat_init = Some (tok, step_value_cst) }
      in
      var_binding vkind binding |> List.map (fun var -> A.VarDecl var)
  in 
  let finalst = vars_or_assign_stmts @ st  in
  [A.For (A.ForClassic (for_init, for_cond, A.Nop), A.Block finalst)]
