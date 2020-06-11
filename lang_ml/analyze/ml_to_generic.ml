(* Yoann Padioleau
 *
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

open Ast_ml
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_ml to AST_generic.
 *
 * See ast_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id = fun x -> x
let option = Common.map_opt
let list = List.map

let string = id
let bool = id
let int = id

let error = G.error
let fake = G.fake

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x
let tok v = info v

let wrap = fun _of_a (v1, v2) ->
  let v1 = _of_a v1 and v2 = info v2 in 
  (v1, v2)

let bracket of_a (t1, x, t2) = (info t1, of_a x, info t2)

let rec ident v = wrap string v

and name (v1, v2) = 
  let v1 = qualifier v1 and v2 = ident v2 in 
  v2, { G.empty_name_info with G.name_qualifier = Some v1 }

and qualifier v = list ident v

and type_ =
  function
  | TyName v1 -> let v1 = name v1 in G.TyName v1
  | TyVar v1 -> let v1 = ident v1 in G.TyVar v1
  | TyFunction ((v1, v2)) -> let v1 = type_ v1 and v2 = type_ v2 in 
                             G.TyFun ([G.param_of_type v1], v2)
  | TyApp ((v1, v2)) -> let v1 = list type_ v1 and v2 = name v2 in
                        G.TyNameApply (v2, v1 |> List.map (fun t -> G.TypeArg t))
  | TyTuple v1 -> let v1 = list type_ v1 in G.TyTuple (G.fake_bracket v1)
  | TyTodo _t -> raise Todo

and expr =
  function
  | L v1 -> let v1 = literal v1 in G.L v1
  | Name v1 -> let v1 = name v1 in G.IdQualified (v1, G.empty_id_info ())
  | Constructor ((v1, v2)) ->
      let v1 = name v1 and v2 = option expr v2 in
      G.Constructor (v1, Common.opt_to_list v2)
  | Tuple v1 -> let v1 = list expr v1 in G.Tuple v1
  | List v1 -> let v1 = bracket (list expr) v1 in G.Container (G.List, v1)
  | Sequence v1 -> let v1 = list expr v1 in G.Seq v1
  | Prefix ((v1, v2)) -> let v1 = wrap string v1 and v2 = expr v2 in
                         let n = v1, G.empty_name_info in
                         G.Call (G.IdQualified (n, G.empty_id_info()), [G.Arg v2])
  | Infix ((v1, v2, v3)) ->
    let n = v2, G.empty_name_info in
    let v1 = expr v1 and v3 = expr v3 in
    G.Call (G.IdQualified (n, G.empty_id_info()), [G.Arg v1; G.Arg v3])

  | Call ((v1, v2)) -> let v1 = expr v1 and v2 = list argument v2 in
                       G.Call (v1, v2)
  | RefAccess ((v1, v2)) -> 
    let v1 = tok v1 and v2 = expr v2 in
    G.DeRef (v1, v2)
  | RefAssign ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = tok v2 and v3 = expr v3 in
      G.Assign (G.DeRef (v2, v1), v2, v3)
  | FieldAccess ((v1, vtok, v2)) -> 
    let v1 = expr v1 in
    let vtok = tok vtok in
    (match v2 with
    | [], id -> let id = ident id in G.DotAccess (v1, vtok, G.FId id)
    | _ -> let v2 = name v2 in G.DotAccess (v1, vtok, G.FName v2)
           
    )
  | FieldAssign ((v1, t1, v2, t2, v3)) ->
    let v1 = expr v1 and v3 = expr v3 in
    let t1 = tok t1 in let t2 = tok t2 in
    (match v2 with
    | [], id -> let id = ident id in 
            G.Assign (G.DotAccess (v1, t1, G.FId id), t2, v3)
    | _ -> let v2 = name v2 in 
           G.Assign (G.DotAccess (v1, t1, G.FName v2), t2, v3)
    )
      
  | Record ((v1, v2)) ->
      let v1 = option expr v1
      and v2 =
        bracket (list (fun (v1, v2) -> let v2 = expr v2 in
          (match v1 with
          | [], id -> let id = ident id in
                      G.basic_field id (Some v2) None
          | _ -> let v1 = name v1 in
                 let e = 
                   G.OtherExpr (G.OE_RecordFieldName, [G.N v1; G.E v2]) in
                 let st = G.ExprStmt e in
                 G.FieldStmt (st)
          )
        ))
          v2
      in 
      let obj = G.Record v2 in
      (match v1 with
      | None -> obj
      | Some e -> G.OtherExpr (G.OE_RecordWith, [G.E e; G.E obj])
      )
  | New ((v1, v2)) -> let v1 = tok v1 and v2 = name v2 in 
                      G.Call (G.IdSpecial (G.New, v1), 
                              [G.Arg (G.IdQualified (v2, G.empty_id_info()))])
  | ObjAccess ((v1, t, v2)) -> 
      let v1 = expr v1 and v2 = ident v2 in
      let t = tok t in
      G.DotAccess (v1, t, G.FId v2)
  | LetIn ((v1, v2, v3)) ->
      let _v1 = list let_binding v1
      and _v2 = expr v2
      and _v3 = rec_opt v3
      in 
      raise Todo
  | Fun ((v1, v2)) -> 
    let v1 = list parameter v1 
    and v2 = expr v2 in 
    let def = { G.fparams = v1; frettype = None; fbody = G.ExprStmt v2 } in
    G.Lambda def

  | If ((_t, v1, v2, v3)) ->
      let v1 = expr v1 and v2 = expr v2 
      and v3 = 
        match v3 with
        | None -> G.L (G.Unit (fake "null"))
        | Some x -> expr x
      in
      G.Conditional (v1, v2, v3)
  | Match ((v1, v2)) ->
      let v1 = expr v1 and v2 = list match_case v2 in
      G.MatchPattern (v1, v2)
  | Try ((t, v1, v2)) ->
      let v1 = expr v1 and v2 = list match_case v2 in 
      let catches = v2 |> List.map (fun (pat, e) -> 
            fake "catch", pat, G.ExprStmt e
        ) in
      let st = G.Try (t, G.ExprStmt v1, catches, None) in
      G.OtherExpr (G.OE_StmtExpr, [G.S st])

  | While ((t, v1, v2)) -> 
    let v1 = expr v1 and v2 = expr v2 in 
    let st = G.While (t, v1, G.ExprStmt v2) in
    G.OtherExpr (G.OE_StmtExpr, [G.S st])
    
  | For ((t, v1, v2, v3, v4, v5)) ->
      let v1 = ident v1
      and v2 = expr v2
      and (tok, nextop, condop) = for_direction v3
      and v4 = expr v4
      and v5 = expr v5
      in 
      let ent = G.basic_entity v1 [] in
      let var = { G.vinit = Some v2; vtype = None } in
      let n = G.IdQualified ((v1, G.empty_name_info), G.empty_id_info()) in
      let next = (G.AssignOp (n, (nextop, tok), G.L (G.Int ("1", tok)))) in
      let cond = G.Call (G.IdSpecial (G.ArithOp condop, tok),
                         [G.Arg n; G.Arg v4]) in
      let header = G.ForClassic ([G.ForInitVar (ent, var)],
                                 Some cond, Some next) in
      let st = G.For (t, header, G.ExprStmt v5) in
      G.OtherExpr (G.OE_StmtExpr, [G.S st])

  | ExprTodo _t -> raise Todo
  
and literal =
  function
  | Int v1 -> let v1 = wrap string v1 in G.Int v1
  | Float v1 -> let v1 = wrap string v1 in G.Float v1
  | Char v1 -> let v1 = wrap string v1 in G.Char v1
  | String v1 -> let v1 = wrap string v1 in G.String v1

and argument =
  function
  | Arg v1 -> let v1 = expr v1 in G.Arg v1
  | ArgKwd ((v1, v2)) -> 
    let v1 = ident v1 and v2 = expr v2 in 
    G.ArgKwd (v1, v2)
  | ArgQuestion ((v1, v2)) -> 
    let v1 = ident v1 and v2 = expr v2 in
    G.ArgOther (G.OA_ArgQuestion, [G.I v1; G.E v2])                          

and match_case (v1, (v2, v3)) =
  let v1 = pattern v1 and v2 = expr v2 and v3 = option expr v3 in
  (match v3 with
  | None -> v1, v2
  | Some x -> G.PatWhen (v1, x), v2
  )

and for_direction =
  function
  | To v1 -> let v1 = tok v1 in v1, G.Plus, G.LtE
  | Downto v1 -> let v1 = tok v1 in v1, G.Minus, G.GtE

and rec_opt v = option tok v

and pattern =
  function
  | PatVar v1 -> let v1 = ident v1 in G.PatId (v1, G.empty_id_info())
  | PatLiteral v1 -> let v1 = literal v1 in G.PatLiteral v1
  | PatConstructor ((v1, v2)) ->
      let v1 = name v1 and v2 = option pattern v2 in
      G.PatConstructor (v1, Common.opt_to_list v2)
  | PatConsInfix ((v1, v2, v3)) ->
      let v1 = pattern v1 and v2 = tok v2 and v3 = pattern v3 in
      let n = ("::", v2), G.empty_name_info in
      G.PatConstructor (n, [v1;v3])
  | PatTuple v1 -> let v1 = list pattern v1 in 
                   G.PatTuple v1
  | PatList v1 -> let v1 = bracket (list pattern) v1 in G.PatList v1
  | PatUnderscore v1 -> let v1 = tok v1 in G.PatUnderscore v1
  | PatRecord v1 ->
      let v1 =
        bracket (list
          (fun (v1, v2) -> let v1 = name v1 and v2 = pattern v2 in v1, v2)) v1
      in 
      G.PatRecord v1
  | PatAs ((v1, v2)) -> 
    let v1 = pattern v1 and v2 = ident v2 in 
    G.PatAs (v1, (v2, G.empty_id_info ()))
  | PatDisj ((v1, v2)) -> 
    let v1 = pattern v1 and v2 = pattern v2 in 
    G.PatDisj (v1, v2)
  | PatTyped ((v1, v2)) -> 
    let v1 = pattern v1 and v2 = type_ v2 in 
    G.PatTyped (v1, v2)

  | PatTodo _t -> raise Todo

and let_binding =
  function
  | LetClassic v1 -> let _v1 = let_def v1 in raise Todo
  | LetPattern ((v1, v2)) -> let v1 = pattern v1 and v2 = expr v2 in 
                             G.LetPattern (v1, v2)

and let_def { lname = lname; lparams = lparams; lbody = lbody } =
  let _v1 = ident lname in
  let _v2 = list parameter lparams in 
  let _v3 = expr lbody in
  ()

and parameter = function
  | Param v -> G.ParamPattern (pattern v)
  | ParamTodo _t -> raise Todo
  
and type_declaration { tname = tname; tparams = tparams; tbody = tbody
                     } =
  let v1 = ident tname in
  let v2 = list type_parameter tparams in
  let v3 = type_def_kind tbody in
  let entity = { (G.basic_entity v1 []) with G.tparams = v2 } in
  let def = { G.tbody = v3 } in
  entity, def

and type_parameter v = ident v, []

and type_def_kind =
  function
  | AbstractType -> G.OtherTypeKind (G.OTKO_AbstractType, [])
  | CoreType v1 -> let v1 = type_ v1 in G.AliasType v1
  | AlgebricType v1 ->
      let v1 =
        list
          (fun (v1, v2) ->
             let v1 = ident v1 and v2 = list type_ v2 in 
             G.OrConstructor (v1, v2))
          v1
      in G.OrType v1
  | RecordType v1 ->
      let v1 =
        bracket (list
          (fun (v1, v2, v3) ->
             let v1 = ident v1
             and v2 = type_ v2
             and v3 = option tok v3
             in 
             let ent = G.basic_entity v1
               (match v3 with 
               | Some tok -> [G.attr G.Mutable tok] 
               | None -> []) in
            G.FieldStmt (G.DefStmt
             (ent, G.FieldDef { G.vinit = None; vtype = Some v2 }))
            ))
          v1
      in G.AndType v1
  
and module_declaration { mname = mname; mbody = mbody } =
  let _v1 = ident mname in 
  let _v2 = module_expr mbody in
  ()

and module_expr =
  function
  | ModuleName v1 -> let _v1 = name v1 in ()
  | ModuleStruct v1 -> let _v1 = list item v1 in ()
  | ModuleTodo _t -> raise Todo


and item =
  function
  | Type v1 -> let _v1 = list type_declaration v1 in ()

  | Exception ((v1, v2)) ->
      let _v1 = ident v1 and _v2 = list type_ v2 in ()
  | External ((v1, v2, v3)) ->
      let _v1 = ident v1
      and _v2 = type_ v2
      and _v3 = list (wrap string) v3
      in ()
  | Open v1 -> let _v1 = name v1 in ()

  | Val ((v1, v2)) -> let _v1 = ident v1 and _v2 = type_ v2 in ()
  | Let ((v1, v2)) ->
      let _v1 = rec_opt v1 and _v2 = list let_binding v2 in ()

  | Module v1 -> let _v1 = module_declaration v1 in ()

  | ItemTodo _t -> raise Todo

and program xs = List.map item xs
