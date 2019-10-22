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

open Ast_python
module G = Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_python to Ast_generic.
 *
 * See ast_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id = fun x -> x
let option = Common.map_opt
let list = List.map
let vref f x = ref (f !x)

let string = id

(*
exception Error of string * Parse_info.info

let error tok msg = 
  raise (Error (msg, tok))
*)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x

let wrap = fun _of_a (v1, v2) ->
  let v1 = _of_a v1 and v2 = info v2 in 
  (v1, v2)

let name v = wrap string v

let dotted_name v = list name v

let resolved_name =
  function
  | LocalVar -> G.Local
  | Parameter -> G.Param
  | GlobalVar -> G.Global [] (* TODO? *)
  | ClassField -> G.NotResolved
  | ImportedModule -> G.ImportedModule
  | ImportedEntity -> G.Global [] (* TODO? *)
  | NotResolved -> G.NotResolved

let expr_context =
  function
  | Load -> ()
  | Store -> ()
  | Del -> ()
  | AugLoad -> ()
  | AugStore -> ()
  | Param -> ()


let rec expr (x: expr) =
  match x with
  | Num v1 -> 
      let v1 = number v1 in 
      (match v1 with
      | Left x -> G.L x
      | Right x -> x
      )
  | Str (v1) -> 
    (match v1 with
    | [x] -> 
      let x = wrap string x in
      G.L (G.String (x))
    | xs -> G.Call (G.IdSpecial G.Concat, 
              xs |> List.map (fun x -> let x = wrap string x in
                  G.Arg (G.L (G.String x))))
    )

  | Name ((v1, v2, v3, v4)) ->
      let v1 = name v1
      and _v2TODO = expr_context v2
      and v3 = option type_ v3
      and v4 = vref resolved_name v4
      in 
      G.Id (v1, 
            { (G.empty_info ()) with 
               G.id_type = ref v3;
               id_resolved = v4 })
          
  | Tuple ((v1, v2)) ->
      let v1 = list expr v1 
      and _v2TODO = expr_context v2 in 
      G.Tuple v1
  | List ((v1, v2)) ->
      let v1 = list expr v1 
      and _v2TODO = expr_context v2 in 
      G.Container (G.List, v1)
  | Subscript ((v1, v2, v3)) ->
      let v1 = expr v1 
      and v2 = list slice v2 
      and _v3TODO = expr_context v3 in 
      (match v2 with
      | [G.OE_SliceIndex, e] -> G.ArrayAccess (v1, e)
      | xs -> 
        G.OtherExpr (G.OE_Slice, 
                     xs |> List.map (fun (other, e) ->
                       G.E (G.OtherExpr (other, [G.E e]))))
      )
  | Attribute ((v1, v2, v3)) ->
      let v1 = expr v1 
      and v2 = name v2 
      and _v3TODO = expr_context v3 in 
      G.ObjAccess (v1, v2)

  | DictOrSet (v) -> 
      let v = list dictorset_elt v in 
      (* less: could be a Set if alls are Key *)
      G.Container (G.Dict, v)
  | ListComp ((v1, v2)) ->
      let v1 = expr v1 
      and v2 = list comprehension v2 in 
      G.OtherExpr (G.OE_ListComp, (G.E v1)::v2)

  | BoolOp ((v1, v2)) -> 
      let v1 = boolop v1 
      and v2 = list expr v2 in 
      G.Call (G.IdSpecial (G.ArithOp v1), v2 |> List.map G.expr_to_arg)
  | BinOp ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = operator v2 and v3 = expr v3 in
      G.Call (G.IdSpecial (G.ArithOp v2), [v1;v3] |> List.map G.expr_to_arg)
  | UnaryOp ((v1, v2)) -> let v1 = unaryop v1 and v2 = expr v2 in 
      (match v1 with
      | Left op ->
            G.Call (G.IdSpecial (G.ArithOp op), [v2] |> List.map G.expr_to_arg)
      | Right oe ->
            G.OtherExpr (oe, [G.E v2])
      )
  | Compare ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = list cmpop v2
      and v3 = list expr v3 in
      (match v2, v3 with
      | [Left op], [e] ->
        G.Call (G.IdSpecial (G.ArithOp op), [v1;e] |> List.map G.expr_to_arg)
      | [Right oe], [e] ->
        G.OtherExpr (oe, [G.E v1; G.E e])
      | _ ->  
        let anyops = 
           v2 |> List.map (function
            | Left arith -> G.E (G.IdSpecial (G.ArithOp arith))
            | Right other -> G.E (G.OtherExpr (other, []))
            ) in
        let any = anyops @ (v3 |> List.map (fun e -> G.E e)) in
        G.OtherExpr (G.OE_CmpOps, any)
      )
  | Call (v1, v2) -> let v1 = expr v1 in let v2 = list argument v2 in 
      G.Call (v1, v2)

  | Lambda ((v1, v2)) -> let v1 = parameters v1 and v2 = expr v2 in 
      G.Lambda (v1, G.ExprStmt v2)
  | IfExp ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in
      G.Conditional (v1, v2, v3)
  | GeneratorExp ((v1, v2)) ->
      let v1 = expr v1 and v2 = list comprehension v2 in 
      G.OtherExpr (G.OE_GeneratorExpr, (G.E v1)::v2)
  | Yield v1 -> let v1 = option expr v1 in
      G.Yield (G.opt_to_nop v1)
  | Repr v1 -> let v1 = expr v1 in
      G.OtherExpr (G.OE_Repr, [G.E v1])

and argument = function
  | Arg e -> let e = expr e in 
      G.Arg e
  | ArgStar e -> let e = expr e in
      G.Arg (G.Call (G.IdSpecial G.Spread, [G.expr_to_arg e]))
  | ArgPow e -> 
      let e = expr e in
      G.ArgOther (G.OA_ArgPow, [G.E e])
  | ArgKwd (n, e) -> let n = name n in let e = expr e in
      G.ArgKwd (n, e)

and dictorset_elt = function
  | KeyVal (v1, v2) -> let v1 = expr v1 in let v2 =  expr v2 in 
      G.Tuple [v1; v2]
  | Key (v1) -> 
      let v1 = expr v1 in
      v1
  | PowInline (v1) -> 
      let v1 = expr v1 in
      G.Call (G.IdSpecial G.Spread, [G.expr_to_arg v1])
  
and number =
  function
  | Int v1     -> let v1 = wrap id v1 in Left (G.Int v1)
  | LongInt v1 -> let v1 = wrap id v1 in Left (G.Int v1)
  | Float v1   -> let v1 = wrap id v1 in Left (G.Float v1)
  | Imag v1    -> 
      let v1 = wrap string v1 in 
      Right (G.OtherExpr (G.OE_Imag, [G.E (G.L (G.Int v1))]))


and boolop = function 
  | And -> G.And
  | Or  -> G.Or

and operator =
  function
  | Add      -> G.Plus
  | Sub      -> G.Minus
  | Mult     -> G.Mult
  | Div      -> G.Div
  | Mod      -> G.Mod
  | Pow      -> G.Pow
  | FloorDiv -> G.FloorDiv
  | LShift   -> G.LSL
  | RShift   -> G.LSR
  | BitOr    -> G.BitOr
  | BitXor   -> G.BitXor
  | BitAnd   -> G.BitAnd

and unaryop = function 
  | Invert -> Right G.OE_Invert
  | Not    -> Left G.Not
  | UAdd   -> Left G.Plus
  | USub   -> Left G.Minus

and cmpop =
  function
  | Eq    -> Left G.Eq
  | NotEq -> Left G.NotEq
  | Lt    -> Left G.Lt
  | LtE   -> Left G.LtE
  | Gt    -> Left G.Gt
  | GtE   -> Left G.GtE
  | Is    -> Right G.OE_Is
  | IsNot -> Right G.OE_IsNot
  | In    -> Right G.OE_In
  | NotIn -> Right G.OE_NotIn

and comprehension (v1, v2, v3) =
  let v1 = expr v1 and v2 = expr v2 and v3 = list expr v3 in
  G.E (G.Tuple [v1; v2; G.Container (G.List, v3)])


and slice =
  function
  | Index v1 -> let v1 = expr v1 in G.OE_SliceIndex, v1
  | Ellipsis -> G.OE_SliceEllipsis, G.Nop
  | Slice ((v1, v2, v3)) ->
      let v1 = option expr v1
      and v2 = option expr v2
      and v3 = option expr v3
      in
      let tuple = G.Tuple ([v1;v2;v3] |> List.map G.opt_to_nop) in
      G.OE_SliceRange, tuple

and parameters xs =
  xs |> List.map (function
   | ParamClassic (e, eopt) ->
    (match e with
    | Name (n, _ctx, typopt, _resolved) ->
      let typopt = option type_ typopt in
      let eopt = option expr eopt in
      let n = name n in
      G.ParamClassic { (G.basic_param n) with
        G.ptype = typopt; pdefault = eopt; }
    | _ -> 
      let e1 = expr e in
      let e2 = option expr eopt |> G.opt_to_nop in
      G.ParamPattern (G.OtherPat (G.OP_Expr, [G.E e1; G.E e2]))
    )
  | ParamStar (n, topt) ->
     let n = name n in
     let topt = option type_ topt in
     G.ParamClassic { (G.basic_param n) with
       G.ptype = topt; pattrs = [G.Variadic]; }
   | ParamPow (n, topt) ->
     let n = name n in
     let topt = option type_ topt in
     G.OtherParam (G.OPO_KwdParam, 
            [G.N n] @ (match topt with None -> [] | Some t -> [G.T t]))
  )
 

and type_ v = 
  let v = expr v in
  G.OtherType (G.OT_Expr, [G.E v])

and type_parent v = 
  let v = argument v in
  G.OtherType (G.OT_Arg, [G.Ar v])

and list_stmt1 xs =
  match (list stmt xs) with
  | [e] -> e
  | xs -> G.Block xs

and stmt x =
  match x with
  | FunctionDef ((v1, v2, v3, v4, v5)) ->
      let v1 = name v1
      and v2 = parameters v2
      and v3 = option type_ v3
      and v4 = list_stmt1 v4
      and v5 = list decorator v5
      in
      let ent = G.basic_entity v1 v5 in
      let def = { G.fparams = v2; frettype = v3; fbody = v4; } in
      (* will be lift up to a IDef later *)
      G.LocalDef (ent, G.FuncDef def)
  | ClassDef ((v1, v2, v3, v4)) ->
      let v1 = name v1
      and v2 = list type_parent v2
      and v3 = list stmt v3
      and v4 = list decorator v4
      in 
      let ent = G.basic_entity v1 v4 in
      let def = { G.ckind = G.Class; cextends = v2; cimplements = [];
                  cbody = List.map G.stmt_to_field v3;
                } in
      (* will be lift up to a IDef later *)
      G.LocalDef (ent, G.ClassDef def)

  (* TODO: should turn some of those in G.LocalDef (G.VarDef ! ) *)
  | Assign ((v1, v2)) -> let v1 = list expr v1 and v2 = expr v2 in
      G.ExprStmt (G.Assign (G.Tuple v1, v2))
  | AugAssign ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = operator v2 and v3 = expr v3 in
      G.ExprStmt (G.AssignOp (v1, v2, v3))
  | Return v1 -> let v1 = option expr v1 in 
      G.Return (G.opt_to_nop v1)

  | Delete v1 -> let v1 = list expr v1 in
      G.OtherStmt (G.OS_Delete, v1 |> List.map (fun x -> G.E x))
  | Print ((v1, v2, v3)) ->
      let v1 = option expr v1
      and v2 = list expr v2
      in
      let tuple = G.Tuple [
          G.opt_to_nop v1;
          G.Container (G.List, v2);
          G.L (G.Bool (v3, Parse_info.fake_info ""))] 
      in
      G.OtherStmt (G.OS_Print, [G.E tuple])

  | If ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = list_stmt1 v2
      and v3 = list_stmt1 v3
      in
      G.If (v1, v2, v3)

  | While ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = list_stmt1 v2
      and v3 = list stmt v3
      in
      (match v3 with
      | [] -> G.While (v1, v2)
      | _ -> G.Block [
              G.While (v1,v2); 
              G.OtherStmt (G.OS_WhileOrElse, v3 |> List.map (fun x -> G.S x))]
      )
            
  | For ((v1, v2, v3, v4)) ->
      let foreach = expr v1
      and ins = expr v2
      and body = list_stmt1 v3
      and orelse = list stmt v4
      in
      let header = G.ForEach (G.OtherPat (G.OP_Expr, [G.E foreach]), ins) in
      (match orelse with
      | [] -> G.For (header, body)
      | _ -> G.Block [
              G.For (header, body);
              G.OtherStmt (G.OS_ForOrElse, orelse|> List.map (fun x -> G.S x))]
      )
  | With ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = option expr v2
      and v3 = list_stmt1 v3
      in
      let anys = [G.E v1; G.E (G.opt_to_nop v2); G.S v3] in
      G.OtherStmt (G.OS_With, anys)

  | Raise (v1) ->
      (match v1 with
      | Some (e, None) -> 
        let e = expr e in G.Throw e
      | Some (e, Some from) -> 
        let e = expr e in
        let from = expr from in
        let st = G.Throw e in
        G.OtherStmt (G.OS_ThrowFrom, [G.E from; G.S st])
      | None ->
        G.OtherStmt (G.OS_ThrowNothing, [])
      )
                  
  | TryExcept ((v1, v2, v3)) ->
      let v1 = list_stmt1 v1
      and v2 = list excepthandler v2
      and orelse = list stmt v3
      in
      (match orelse with
      | [] -> G.Try (v1, v2, None)
      | _ -> G.Block [
              G.Try (v1, v2, None);
              G.OtherStmt (G.OS_TryOrElse, orelse |> List.map (fun x -> G.S x))
              ]
      )

  | TryFinally ((v1, v2)) ->
      let v1 = list_stmt1 v1 and v2 = list_stmt1 v2 in
      (* could lift down the Try in v1 *)
      G.Try (v1, [], Some v2)

  | Assert ((v1, v2)) -> let v1 = expr v1 and v2 = option expr v2 in
      G.Assert (v1, v2)

  | Import v1 -> let v1 = list alias2 v1 in 
      G.Block (v1 |> List.map (fun (dotted, nopt) ->
            G.LocalDirective (G.ImportAll (G.DottedName dotted, nopt))))

  | ImportFrom ((v1, v2, v3)) ->
      let v1 = dotted_name v1
      and v2 = list alias v2
      and _v3Dotlevel = (*option int v3 *) v3
      in
      (* will be lift up to IDef later *)
      G.LocalDirective (G.Import (G.DottedName v1, v2))

  | Global v1 -> let v1 = list name v1 in
      G.OtherStmt (G.OS_Global, v1 |> List.map (fun x -> G.N x))

  | ExprStmt v1 -> let v1 = expr v1 in G.ExprStmt v1

  | Pass -> G.OtherStmt (G.OS_Pass, [])
  | Break -> G.Break (None)
  | Continue -> G.Continue (None)


and excepthandler =
  function
  | ExceptHandler ((v1, v2, v3)) ->
      let v1 = option expr v1
      and v2 = option expr v2
      and v3 = list_stmt1 v3
      in 
      (match v1, v2 with
      | Some e, None ->
        let pat = G.OtherPat (G.OP_Expr, [G.E e]) in
        pat, v3
      | _ ->
        let e1 = G.opt_to_nop v1 in
        let e2 = G.opt_to_nop v2 in
        let pat = G.OtherPat (G.OP_Expr, [G.E e1; G.E e2]) in
        pat, v3
      )

and decorator v = 
  let v = expr v in
  G.OtherAttribute (G.OA_Expr, [G.E v])

and alias (v1, v2) = 
  let v1 = name v1 and v2 = option name v2 in 
  v1, v2
and alias2 (v1, v2) = 
  let v1 = dotted_name v1 and v2 = option name v2 in
  v1, v2

let program v = 
  let v = list stmt v in
  v |> List.map G.stmt_to_item

(*
let any =
  function
  | Expr v1 -> let v1 = expr v1 in ()
  | Stmt v1 -> let v1 = stmt v1 in ()
  | Stmts v1 -> let v1 = list stmt v1 in ()
  | Program v1 -> let v1 = program v1 in ()
*)
