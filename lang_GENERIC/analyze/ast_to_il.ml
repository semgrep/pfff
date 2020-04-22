(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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

open Il
module G = Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST generic to IL translation.
 *
 * todo:
 *  - a lot ...
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = {
  instrs: instr list ref;
}

let empty_env () = {
  instrs = ref [];
}


(*****************************************************************************)
(* Error management *)
(*****************************************************************************)
let error tok s =
  raise (Parse_info.Ast_builder_error (s, tok))

let warning tok s =
  pr2 (spf "%s: %s" (Parse_info.string_of_info tok) s)

let error_any any_generic msg =
  let toks = Lib_ast.ii_of_any any_generic in
  let s = Meta_ast.vof_any any_generic |> Ocaml.string_of_v in
  error (List.hd toks) (spf "%s: %s" msg s)

let sgrep_construct any_generic =
  error_any any_generic "Sgrep Construct"

let todo any_generic =
  error_any any_generic "TODO Construct"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let fresh_var _env tok = 
  let i = G.gensym () in
  ("_tmp", tok), i
let _fresh_label _env tok = 
  let i = G.gensym () in
  ("_label", tok), i
let fresh_lval env tok =
  let var = fresh_var env tok in
  { base = Var var; offset = NoOffset }

let lval_of_id_info _env id id_info =
  let sid = 
    match !(id_info.G.id_resolved) with
    | Some (_resolved, sid) -> sid
    | None -> 
        warning (snd id) (spf "the ident '%s' is not resolved" (fst id));
        -1
  in
  let var = id, sid in
  { base = Var var; offset = NoOffset }
let lval_of_ent env ent = 
  lval_of_id_info env ent.G.name ent.G.info

(* TODO: should do first pass on body to get all labels and assign
 * a gensym to each.
 *)
let label_of_label _env lbl =
  lbl, -1
let lookup_label _env lbl =
  lbl, -1

let mk_e e eorig = 
  { e; eorig}
let mk_i i iorig =
  { i; iorig }
let mk_s s =
  { s }

let add_instr env instr = 
  Common.push instr env.instrs

(*****************************************************************************)
(* lvalue *)
(*****************************************************************************)
let rec _lval _env _x =
  raise Todo

(*****************************************************************************)
(* Assign *)
(*****************************************************************************)
and assign env lhs _tok rhs_exp eorig =
  match lhs with
  | G.Id (id, id_info) ->
      let lval = lval_of_id_info env id id_info in
      add_instr env (mk_i (Set (lval, rhs_exp)) eorig);
      mk_e (Lvalue lval) lhs
  | _ -> todo (G.E lhs)

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
and expr env eorig =
  match eorig with
  | G.Call (G.IdSpecial (G.ArithOp op, tok), args) ->
      let args = arguments env args in
      mk_e (Operator ((op, tok), args)) eorig
  | G.Call (G.IdSpecial (G.IncrDecr (_incdec, _prepost), _tok), _args) ->
      todo (G.E eorig)
  (* todo: if the xxx_to_generic forgot to generate Eval *)
  | G.Call (G.Id (("eval", tok), { G.id_resolved = {contents = None}; _}), 
      args) ->
      let lval = fresh_lval env tok in
      let special = Eval, tok in
      let args = arguments env args in
      add_instr env (mk_i (CallSpecial (Some lval, special, args)) eorig);
      mk_e (Lvalue lval) eorig
  | G.Call (G.IdSpecial spec, args) ->
      let tok = snd spec in
      let lval = fresh_lval env tok in
      let special = call_special env spec in
      let args = arguments env args in
      add_instr env (mk_i (CallSpecial (Some lval, special, args)) eorig);
      mk_e (Lvalue lval) eorig

  | G.L lit -> mk_e (Literal lit) eorig
  | G.Id (id, id_info) -> 
      let lval = lval_of_id_info env id id_info in
      mk_e (Lvalue lval) eorig

  | G.Assign (e1, tok, e2) ->
      let exp = expr env e2 in
      assign env e1 tok exp eorig

  | G.Seq xs ->
      (match List.rev xs with
      | [] -> raise Impossible
      | last::xs ->
         let xs = List.rev xs in
         xs |> List.iter (fun e -> let _eIGNORE = expr env e in ());
         expr env last
      )
                
      
  | _ -> todo (G.E eorig)
  

and expr_opt env = function
  | None -> 
      let void = G.Unit (G.fake "void") in
      mk_e (Literal void) (G.L void)
  | Some e -> expr env e

and call_special _env (x, tok) = 
  (match x with
  | G.ArithOp _ | G.IncrDecr _ -> 
        raise Impossible (* should be intercepted before *)
  | G.This | G.Super | G.Self | G.Parent ->
        raise Impossible (* should be intercepted before *)
  | G.Eval -> Eval
  | G.Typeof -> Typeof | G.Instanceof -> Instanceof | G.Sizeof -> Sizeof
  | G.New -> New 
  | G.Concat -> Concat | G.Spread -> Spread
  | G.EncodedString _ -> todo (G.E (G.IdSpecial (x, tok)))
  ), tok

(* TODO: dependency of order between arguments for instr? *)
and arguments env xs = 
  xs |> List.map (argument env)

and argument env arg =
  match arg with
  | G.Arg e -> expr env e
  | _ -> todo (G.Ar arg)


(*****************************************************************************)
(* Pattern *)
(*****************************************************************************)
and pattern env pat =
  match pat with
  | G.PatId (id, id_info) ->
      Left (lval_of_id_info env id id_info)
  | _ -> todo (G.P pat)

and pattern_assign_statements env exp eorig pat =
  let lval = 
     match pattern env pat with
     | Left l -> l
     | Right _ -> todo (G.P pat)
   in
   [mk_s (Instr (mk_i (Set (lval, exp)) eorig))]

(*****************************************************************************)
(* Exprs and instrs *)
(*****************************************************************************)

(* just to ensure the code after does not call expr directly *)
let expr_orig = expr
let expr () = ()

let expr_and_instrs env e =
  ignore(expr ());
  let e = expr_orig env e in
  let xs = List.rev !(env.instrs) in
  env.instrs := [];
  e, (xs |> List.map (fun instr -> mk_s (Instr instr)))

let expr_and_instrs_opt env eopt =
  match eopt with
  | None -> expr_opt env None, []
  | Some e -> expr_and_instrs env e

let for_var_or_expr_list env xs =
  xs |> List.map (function
   | G.ForInitExpr e -> 
        let _eIGNORE, ss = expr_and_instrs env e in
        ss
   | G.ForInitVar (ent, vardef) ->
      (* copy paste of VarDef case in stmt *)
      (match vardef with
      | { G.vinit = Some e; vtype = _typTODO} ->
         let e', ss = expr_and_instrs env e in
         let lv = lval_of_ent env ent in
         ss @ [mk_s (Instr (mk_i (Set (lv, e')) e))]; 
      | _ -> []
      )
  ) |> List.flatten

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
let rec stmt env st =
  match st with
  | G.ExprStmt e ->
      (* optimize? pass context to expr when no need for return value? *)
      let _eIGNORE, ss = expr_and_instrs env e in
      ss

  | G.DefStmt (ent, G.VarDef { G.vinit = Some e; vtype = _typTODO}) ->
    let e', ss = expr_and_instrs env e in
    let lv = lval_of_ent env ent in
    ss @ [mk_s (Instr (mk_i (Set (lv, e')) e))]; 
  | G.DefStmt def -> [mk_s (DefStmt def)]
  | G.DirectiveStmt dir -> [mk_s (DirectiveStmt dir)]

  | G.Block xs -> List.map (stmt env) xs |> List.flatten

  | G.If (tok, e, st1, st2) ->
    let e', ss = expr_and_instrs env e in
    let st1 = stmt env st1 in
    let st2 = stmt env st2 in
    ss @ [mk_s (If (tok, e', st1, st2))]

  | G.Switch (_, _, _) -> todo (G.S st)

  | G.While(tok, e, st) ->
    let e', ss = expr_and_instrs env e in
    let st = stmt env st in
    ss @ [mk_s (Loop (tok, e', st @ ss))]
  | G.DoWhile(tok, st, e) ->
    let st = stmt env st in
    let e', ss = expr_and_instrs env e in
    st @ ss @ [mk_s (Loop (tok, e', st @ ss))]

  | G.For (tok, G.ForEach (pat, tok2, e), st) -> 
      let e', ss = expr_and_instrs env e in
      let st = stmt env st in

      let next_lval = fresh_lval env tok2 in
      let hasnext_lval = fresh_lval env tok2 in
      let hasnext_call = mk_s (Instr (
        mk_i (CallSpecial (Some hasnext_lval, (ForeachHasNext, tok2), [e']))e))
      in
      let next_call = mk_s (Instr(
        mk_i (CallSpecial (Some next_lval, (ForeachNext, tok2), [e'])) e))
      in
      (* todo? same semantic? or need to take Ref? or pass lval
       * directly in next_call instead of using intermediate next_lval?
       *)
      let assign = 
        pattern_assign_statements env (mk_e (Lvalue next_lval) e) e pat in
      let cond = mk_e (Lvalue hasnext_lval) e in

      (ss @ [hasnext_call]) @
      [mk_s (Loop(tok, cond, [next_call] @ assign @
              st @ ((* ss @ ?*) [hasnext_call])))]
      
  | G.For (tok, G.ForClassic (xs, eopt1, eopt2), st) 
   -> 
      let ss1 = for_var_or_expr_list env xs in
      let st = stmt env st in
      let cond, ss2 =
        match eopt1 with
        | None -> 
            let vtrue = G.Bool (true, tok) in
            mk_e (Literal (vtrue)) (G.L vtrue), []
        | Some e -> expr_and_instrs env e
      in
      let next =
        match eopt2 with
        | None -> []
        | Some e -> 
            let _eIGNORE, ss = expr_and_instrs env e in
            ss
      in
      ss1 @ ss2 @ 
      [mk_s (Loop(tok, cond, st @ next @ ss2))]

  (* TODO: repeat env work of controlflow_build.ml *)
  | G.Continue (_, _) | G.Break (_, _)
   -> todo (G.S st)
    
  | G.Label (lbl, st) ->
      let lbl = label_of_label env lbl in
      let st = stmt env st in
      [mk_s (Label lbl)] @ st
  | G.Goto (tok, lbl) ->
      let lbl = lookup_label env lbl in
      [mk_s (Goto (tok, lbl))]

  | G.Return (tok, eopt) ->
      let e, ss = expr_and_instrs_opt env eopt in
      ss @ [mk_s (Return (tok, e))]

  | G.Assert (tok, e, eopt) ->
      let e', ss1 = expr_and_instrs env e in
      let eopt', ss2 = expr_and_instrs_opt env eopt in
      let special = Assert, tok in
      (* less: wrong e? would not be able to match on Assert, or 
       * need add sorig:
       *)
      ss1 @ ss2 @
      [mk_s (Instr (mk_i (CallSpecial (None, special, [e'; eopt'])) e))]

  | G.Throw (tok, e) ->
      let e, ss = expr_and_instrs env e in
      ss @ [mk_s (Throw (tok, e))]
  | G.Try (_, _, _, _) 
   -> todo (G.S st)
      
  | G.DisjStmt _ -> sgrep_construct (G.S st)
  | G.OtherStmt _ | G.OtherStmtWithStmt _ -> todo (G.S st)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let stmt st =
  let env = empty_env () in
  stmt env st
