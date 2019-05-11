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
(* Cst_js to Ast_js *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* not used for now *)
type _env = unit

let empty_env () = ()

exception TodoConstruct of string * Parse_info.info
(* The string is often "advanced es6" or "Typescript" usually *)
exception UnhandledConstruct of string * Parse_info.info

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let opt f env x =
  match x with
  | None -> None
  | Some x -> Some (f env x)

let rec comma_list = function
  | [] -> []
  | Common.Left x  :: rl -> x :: comma_list rl
  | Common.Right _ :: rl -> comma_list rl

let paren (_, x, _) = x
let fst3 (x, _, _) = x

let noop = A.Block []


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rec program xs =
  let env = empty_env () in
  module_items env xs

and module_items env xs =
  xs |> List.map (module_item env) |> List.flatten

and module_item env = function
  | C.It x -> item env x
  | C.Import (tok, _, _)
  | C.Export (tok, _)
     -> raise (TodoConstruct ("namespace", tok))

and item env = function
  | C.St x -> stmt env x
  | C.FunDecl x -> 
    let fun_ = func_decl env x in
    (match x.C.f_name with
    | Some x ->
      [A.VarDecl {A.v_name = name env x; v_kind = Const; v_init = Fun fun_}]
    | None ->
       raise (UnhandledConstruct ("weird anon func decl", fst3 x.C.f_params))
    )
  | C.ClassDecl x -> 
    let class_ = class_decl env x in
    (match x.C.c_name with
    | Some x ->
      [A.VarDecl {A.v_name = name env x; v_kind=Const;v_init=A.Class class_}]
    | None ->
       raise (UnhandledConstruct ("weird anon class decl", x.C.c_tok))
    )

  | C.InterfaceDecl x -> 
    raise (UnhandledConstruct ("Typescript", x.C.i_tok))
  | C.ItemTodo tok ->
    raise (TodoConstruct ("Todo", tok))

(* ------------------------------------------------------------------------- *)
(* Names *)
(* ------------------------------------------------------------------------- *)
and name env x = x
and label env x = x

and property_name env = function
  | C.PN_Id x       -> A.PN (name env x)
  | C.PN_String x   -> A.PN (name env x)
  | C.PN_Num x      -> A.PN (name env x)
  | C.PN_Computed x -> A.PN_Computed (x |> paren |> expr env)

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)

and stmt env = function
  | C.VarsDecl (_vkind, _vbindings, _) ->
    raise Todo
  | C.Block x -> 
    [A.Block (x |> paren |> List.map (item env) |> List.flatten)]
  | C.Nop _ -> 
    []
  | C.ExprStmt (e, _) ->
    [A.ExprStmt (expr env e)]
  | C.If (_, e, then_, elseopt) ->
    let e = e |> paren |> expr env in
    let then_ = stmt1 env then_ in
    let else_ = 
      match elseopt with
      | None -> noop
      | Some (_, st) -> stmt1 env st
    in
    [A.If (e, then_, else_)]
  | C.Do (_, st, _, e, _) ->
     let st = stmt1 env st in
     let e = e |> paren |> expr env in
     [A.Do (st, e)]
  | C.While (_, e, st) ->
     let e = e |> paren |> expr env in
     let st = stmt1 env st in
     [A.While (e, st)]
  | C.For (_, _, lhs_vars_opt, _, e2opt, _, e3opt, _, st) ->
     let e1 = 
       raise Todo 
     in
     let e2 = expr_opt env e2opt in
     let e3 = expr_opt env e3opt in
     let st = stmt1 env st in
     [A.For (A.ForClassic (e1, e2, e3), st)]
  | C.ForIn (_, _, lhs_var, _, e2, _, st) ->
    let e1 =
      raise Todo
    in 
    let e2 = expr env e2 in
    let st = stmt1 env st in
    [A.For (A.ForIn (e1, e2), st)]
  | C.ForOf (_, _, lhs_var, _, e2, _, st) ->
    let e1 =
      raise Todo
    in 
    let e2 = expr env e2 in
    let st = stmt1 env st in
    [A.For (A.ForOf (e1, e2), st)]
  | C.Switch (_, e, xs) ->
    let e = e |> paren |> expr env in
    let xs = xs |> paren |> List.map (case_clause env) in
    [A.Switch (e, xs)]
  | C.Continue (_, lopt, _) -> 
    [A.Continue (opt label env lopt)]
  | C.Break (_, lopt, _) -> 
    [A.Break (opt label env lopt)]
  | C.Return (_, eopt, _) -> 
    [A.Return (expr_opt env eopt)]
  | C.With (tok, e, st) ->
    raise (TodoConstruct ("with", tok))
  | C.Labeled (lbl, _, st) ->
    let lbl = label env lbl in
    let st = stmt1 env st in
    [A.Label (lbl, st)]
  | C.Throw (_, e, _) ->
    let e = expr env e in
    [A.Throw e]
  | C.Try (_, st, catchopt, finally_opt) ->
    let st = stmt1 env st in
    let catchopt = opt (fun env (_, arg, st) ->
       let arg = paren arg in
       let st = stmt1 env st in
       (arg, st)
       ) env catchopt in
    let finally_opt = opt (fun env (_, st) -> stmt1 env st) env finally_opt in
    [A.Try (st, catchopt, finally_opt)]

and stmt1 env st =
  match stmt env st with
  | [] -> A.Block []
  | [x] -> x
  | xs -> A.Block xs

and case_clause env = function
  | C.Default (_, _, xs) -> A.Default (stmt1_item_list env xs)
  | C.Case (_, e, _, xs) ->
    let e = expr env e in
    A.Case (e, stmt1_item_list env xs)

and stmt1_item_list env items =
  match items |> List.map (item env) |> List.flatten with
  | [] -> A.Block []
  | [x] -> x
  | xs -> A.Block xs
  
(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
and expr env = function
  | C.L x -> literal env x
  | C.V (s, tok) -> 
     (match s with
     | "eval" -> A.IdSpecial (A.Eval, tok)
     | "undefined" -> A.IdSpecial (A.Undefined, tok)
     (* todo? require? import? *)
     | _ -> A.Id (s, tok)
     )
  | C.This tok -> A.IdSpecial (A.This, tok)
  | C.Super tok -> A.IdSpecial (A.Super, tok)
 
  | C.U ((op, tok), e) ->
    let special = unop env op in
    let e = expr env e in
    A.Apply (A.IdSpecial (special, tok), [e])
  | C.B (e1, op, e2) ->
    let e1 = expr env e1 in
    let e2 = expr env e2 in
    binop env op e1 e2
  | C.Period (e, _, n) ->
    let e = expr env e in
    A.ObjAccess (e, PN (name env n))
  | C.Bracket (e, e2) ->
    let e = expr env e in
    A.ObjAccess (e, PN_Computed (expr env (paren e2)))
  | C.XhpHtml x ->
    let tok = 
      match x with
      | C.Xhp ((_, tok), _, _, _, _) -> tok
      | C.XhpSingleton ((_, tok), _, _) -> tok
    in
    raise (UnhandledConstruct ("xhp", tok))
  | C.Paren x ->
     expr env (paren x)
  | _ -> raise Todo

and expr_opt env = function
  | None -> A.Nop
  | Some e -> expr env e

and literal env = function
  | C.Bool x -> A.Bool x
  | C.Num x -> A.Num x
  | C.String x -> A.String x
  | C.Regexp x -> A.Regexp x
  | C.Null tok -> A.IdSpecial (A.Null, tok)

and unop env = function
  | C.U_new -> A.New
  | C.U_delete -> A.Delete
  | C.U_typeof -> A.Typeof
  | C.U_void  -> A.Void
  | C.U_pre_increment -> A.Incr (true)
  | C.U_pre_decrement -> A.Decr (true)
  | C.U_post_increment -> A.Incr (false)
  | C.U_post_decrement -> A.Decr (false)
  | C.U_plus -> A.Plus
  | C.U_minus -> A.Minus
  | C.U_not -> A.Not 
  | C.U_bitnot -> A.BitNot
  | C.U_spread -> A.Spread

and binop env (op,tok) e1 e2 = 
  let res = 
    match op with
    | C.B_instanceof -> Left A.Instanceof
    | C.B_in -> Left A.In
    | C.B_add -> Left A.Plus
    | C.B_sub -> Left A.Minus
    | C.B_mul -> Left A.Mul
    | C.B_div -> Left A.Div
    | C.B_mod -> Left A.Mod
    | C.B_expo -> Left A.Expo
    | C.B_lt -> Left A.Lower
    | C.B_gt -> Left A.Greater
    | C.B_lsr -> Left A.Lsr
    | C.B_asr -> Left A.Asr
    | C.B_lsl -> Left A.Lsl
    | C.B_bitand -> Left A.BitAnd
    | C.B_bitor -> Left A.BitOr
    | C.B_bitxor -> Left A.BitXor
    | C.B_and -> Left A.And
    | C.B_or -> Left A.Or
    | C.B_equal -> Left A.Equal
    | C.B_physequal -> Left A.PhysEqual

    (* todo: e1 and e2 can have side effect, need intermediate var *)
    | C.B_le -> Right (
      A.Apply (A.IdSpecial (A.Or, tok), [
          A.Apply (A.IdSpecial (A.Lower, tok), [e1;e2]);
          A.Apply (A.IdSpecial (A.Equal, tok), [e1;e2]);
        ]))
    | C.B_ge -> Right (
      A.Apply (A.IdSpecial (A.Or, tok), [
          A.Apply (A.IdSpecial (A.Greater, tok), [e1;e2]);
          A.Apply (A.IdSpecial (A.Equal, tok), [e1;e2]);
        ]))

    | C.B_notequal -> Right (
      A.Apply (A.IdSpecial (A.Not, tok), [
        A.Apply (A.IdSpecial (A.Equal, tok), [e1;e2]);]))
    | C.B_physnotequal -> Right (
      A.Apply (A.IdSpecial (A.Not, tok), [
        A.Apply (A.IdSpecial (A.PhysEqual, tok), [e1;e2]);]))
  in
  match res with
  | Left special ->A.Apply (A.IdSpecial (special, tok), [e1; e2])
  | Right x -> x


(* ------------------------------------------------------------------------- *)
(* Entities *)
(* ------------------------------------------------------------------------- *)

and func_decl env x =
  raise Todo

and class_decl env x =
  raise Todo


(* ------------------------------------------------------------------------- *)
(* Misc *)
(* ------------------------------------------------------------------------- *)
