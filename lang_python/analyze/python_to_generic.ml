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
let bool = id

let fake_info () = Parse_info.fake_info "FAKE"

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x

let wrap = fun _of_a (v1, v2) ->
  let v1 = _of_a v1 and v2 = info v2 in 
  (v1, v2)

let name v = wrap string v

let dotted_name v = list name v

let gensym_TODO = -1 

let resolved_name name =
  function
  | LocalVar -> Some (G.Local gensym_TODO)
  | Parameter -> Some (G.Param gensym_TODO)
  | GlobalVar -> Some (G.Global [name])
  | ClassField -> None
  | ImportedModule xs -> Some (G.ImportedModule xs)
  | ImportedEntity xs -> Some (G.Global xs)
  | NotResolved -> None

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
  | Bool v1 -> 
    let v1 = wrap bool v1 in
     G.L (G.Bool v1)
  | None_ x ->
     let x = info x in
     G.L (G.Null x)
  | Ellipses x ->
     let x = info x in
     G.Ellipses x
  | Num v1 -> 
      let v1 = number v1 in 
      (match v1 with
      | Left x -> G.L x
      | Right x -> x
      )
  | Str (v1) -> 
      let v1 = wrap string v1 in
      G.L (G.String (v1))

  | InterpolatedString xs ->
    G.Call (G.IdSpecial (G.Concat, fake_info ()), 
      xs |> List.map (fun x -> let x = expr x in G.Arg (x))
    )
  | TypedExpr (v1, v2) ->
     let v1 = expr v1 in
     let v2 = type_ v2 in
     G.Cast (v2, v1)
  | ExprStar v1 ->
    let v1 = expr v1 in
    G.Call (G.IdSpecial (G.Spread, fake_info()), [G.expr_to_arg v1])

  | Name ((v1, v2, v3)) ->
      let v1 = name v1
      and _v2TODO = expr_context v2
      and v3 = vref (resolved_name v1) v3
      in 
      G.Name ((v1, G.empty_name_info),
               { G.id_type = ref None;
                 id_resolved = v3 })
          
  | Tuple ((CompList v1, v2)) ->
      let v1 = list expr v1 
      and _v2TODO = expr_context v2 in 
      G.Tuple v1

  | Tuple ((CompForIf (v1, v2), v3)) ->
      let e1 = comprehension expr v1 v2 in
      let _v4TODO = expr_context v3 in 
      G.Tuple e1

  | List ((CompList v1, v2)) ->
      let v1 = list expr v1 
      and _v2TODO = expr_context v2 in 
      G.Container (G.List, v1)

  | List ((CompForIf (v1, v2), v3)) ->
      let e1 = comprehension expr v1 v2 in
      let _v3TODO = expr_context v3 in 
      G.Container (G.List, e1)

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

  | DictOrSet (CompList v) -> 
      let v = list dictorset_elt v in 
      (* less: could be a Set if alls are Key *)
      G.Container (G.Dict, v)

  | DictOrSet (CompForIf (v1, v2)) -> 
      let e1 = comprehension2 dictorset_elt v1 v2 in
      G.Container (G.Dict, e1)

  | BoolOp (((v1,tok), v2)) -> 
      let v1 = boolop v1 
      and v2 = list expr v2 in 
      G.Call (G.IdSpecial (G.ArithOp v1, tok), v2 |> List.map G.expr_to_arg)
  | BinOp ((v1, (v2, tok), v3)) ->
      let v1 = expr v1 and v2 = operator v2 and v3 = expr v3 in
      G.Call (G.IdSpecial (G.ArithOp v2, tok), [v1;v3] |> List.map G.expr_to_arg)
  | UnaryOp (((v1, tok), v2)) -> let v1 = unaryop v1 and v2 = expr v2 in 
      (match v1 with
      | Left op ->
            G.Call (G.IdSpecial (G.ArithOp op, tok), [v2] |> List.map G.expr_to_arg)
      | Right oe ->
            G.OtherExpr (oe, [G.E v2])
      )
  | Compare ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = list cmpop v2
      and v3 = list expr v3 in
      (match v2, v3 with
      | [Left op, tok], [e] ->
        G.Call (G.IdSpecial (G.ArithOp op, tok), [v1;e] |> List.map G.expr_to_arg)
      | [Right oe, _tok], [e] ->
        G.OtherExpr (oe, [G.E v1; G.E e])
      | _ ->  
        let anyops = 
           v2 |> List.map (function
            | Left arith, tok -> G.E (G.IdSpecial (G.ArithOp arith, tok))
            | Right other, _tok -> G.E (G.OtherExpr (other, []))
            ) in
        let any = anyops @ (v3 |> List.map (fun e -> G.E e)) in
        G.OtherExpr (G.OE_CmpOps, any)
      )
  | Call (v1, v2) -> let v1 = expr v1 in let v2 = list argument v2 in 
      G.Call (v1, v2)

  | Lambda ((v1, v2)) -> let v1 = parameters v1 and v2 = expr v2 in 
      G.Lambda ({G.fparams = v1; fbody = G.ExprStmt v2; frettype = None})
  | IfExp ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in
      G.Conditional (v1, v2, v3)
  | Yield v1 -> let v1 = option expr v1 in
      G.Yield (G.opt_to_nop v1)
  | Await v1 -> let v1 = expr v1 in
      G.Await v1
  | Repr v1 -> let v1 = expr v1 in
      G.OtherExpr (G.OE_Repr, [G.E v1])

and argument = function
  | Arg e -> let e = expr e in 
      G.Arg e
  | ArgStar e -> let e = expr e in
      G.Arg (G.Call (G.IdSpecial (G.Spread, fake_info()), [G.expr_to_arg e]))
  | ArgPow e -> 
      let e = expr e in
      G.ArgOther (G.OA_ArgPow, [G.E e])
  | ArgKwd (n, e) -> let n = name n in let e = expr e in
      G.ArgKwd (n, e)
  | ArgComp (e, xs) ->
      let e = expr e in
      G.ArgOther (G.OA_ArgComp, (G.E e)::list for_if xs)

and for_if = function
  | CompFor (e1, e2) -> 
      let e1 = expr e1 in let e2 = expr e2 in
      G.E (G.OtherExpr (G.OE_CompFor, [G.E e1; G.E e2]))
  | CompIf (e1) -> 
      let e1 = expr e1 in
      G.E (G.OtherExpr (G.OE_CompIf, [G.E e1]))


and dictorset_elt = function
  | KeyVal (v1, v2) -> let v1 = expr v1 in let v2 =  expr v2 in 
      G.Tuple [v1; v2]
  | Key (v1) -> 
      let v1 = expr v1 in
      v1
  | PowInline (v1) -> 
      let v1 = expr v1 in
      G.Call (G.IdSpecial (G.Spread, fake_info()), [G.expr_to_arg v1])
  
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

and cmpop (a,b) =
  match a with
  | Eq    -> Left G.Eq, b
  | NotEq -> Left G.NotEq, b
  | Lt    -> Left G.Lt, b
  | LtE   -> Left G.LtE, b
  | Gt    -> Left G.Gt, b
  | GtE   -> Left G.GtE, b
  | Is    -> Right G.OE_Is, b
  | IsNot -> Right G.OE_IsNot, b
  | In    -> Right G.OE_In, b
  | NotIn -> Right G.OE_NotIn, b

and comprehension f v1 v2 =
  let v1 = f v1 in
  let v2 = list for_if v2 in
  [G.OtherExpr (G.OE_CompForIf, (G.E v1)::v2)]

and comprehension2 f v1 v2 =
  let v1 = f v1 in
  let v2 = list for_if v2 in
  [G.OtherExpr (G.OE_CompForIf, (G.E v1)::v2)]

and slice =
  function
  | Index v1 -> let v1 = expr v1 in G.OE_SliceIndex, v1
  | Slice ((v1, v2, v3)) ->
      let v1 = option expr v1
      and v2 = option expr v2
      and v3 = option expr v3
      in
      let tuple = G.Tuple ([v1;v2;v3] |> List.map G.opt_to_nop) in
      G.OE_SliceRange, tuple

and parameters xs =
  xs |> List.map (function
  | ParamClassic ((n, topt), eopt) ->
     let n = name n in
     let topt = option type_ topt in
     let eopt = option expr eopt in
     G.ParamClassic { (G.basic_param n) with G.ptype = topt; pdefault = eopt; }
  | ParamStar (n, topt) ->
     let n = name n in
     let topt = option type_ topt in
     G.ParamClassic { (G.basic_param n) with
       G.ptype = topt; pattrs = [G.Variadic]; }
   | ParamPow (n, topt) ->
     let n = name n in
     let topt = option type_ topt in
     G.OtherParam (G.OPO_KwdParam, 
            [G.Id n] @ (match topt with None -> [] | Some t -> [G.T t]))
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
      G.DefStmt (ent, G.FuncDef def)
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
      G.DefStmt (ent, G.ClassDef def)

  (* TODO: should turn some of those in G.LocalDef (G.VarDef ! ) *)
  | Assign ((v1, v2)) -> let v1 = list expr v1 and v2 = expr v2 in
      (match v1 with
      | [] -> raise Impossible
      | [a] -> G.ExprStmt (G.Assign (a, v2))
      | xs -> G.ExprStmt (G.Assign (G.Tuple xs, v2)) 
      )
  | AugAssign ((v1, (v2, tok), v3)) ->
      let v1 = expr v1 and v2 = operator v2 and v3 = expr v3 in
      G.ExprStmt (G.AssignOp (v1, (v2, tok), v3))
  | Return v1 -> let v1 = option expr v1 in 
      G.Return (G.opt_to_nop v1)

  | Delete v1 -> let v1 = list expr v1 in
      G.OtherStmt (G.OS_Delete, v1 |> List.map (fun x -> G.E x))
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
      let e =
        match v2 with
        | None -> v1
        | Some e2 -> G.LetPattern (G.OtherPat (G.OP_Expr, [G.E e2]), v1)
      in
      G.OtherStmtWithStmt (G.OSWS_With, e, v3)

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
            G.DirectiveStmt (G.ImportAs (G.DottedName dotted, nopt))))

  | ImportFrom ((v1, v2, v3)) ->
      let v1 = dotted_name v1
      and v2 = list alias v2
      and _v3Dotlevel = (*option int v3 *) v3
      in
      (* will be lift up to IDef later *)
      G.DirectiveStmt (G.ImportFrom (G.DottedName v1, v2))

  | Global v1 -> let v1 = list name v1 in
      G.OtherStmt (G.OS_Global, v1 |> List.map (fun x -> G.Id x))
  | NonLocal v1 -> let v1 = list name v1 in
      G.OtherStmt (G.OS_NonLocal, v1 |> List.map (fun x -> G.Id x))

  | ExprStmt v1 -> let v1 = expr v1 in G.ExprStmt v1

  | Async x ->
      let x = stmt x in
      (match x with
      | G.DefStmt (ent, func) ->
          G.DefStmt ({ ent with G.attrs = G.Async::ent.G.attrs}, func)
      | _ -> G.OtherStmt (G.OS_Async, [G.S x])
      )

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

let any =
  function
  | Expr v1 -> let v1 = expr v1 in G.E v1
  | Stmt v1 -> let v1 = stmt v1 in G.S v1
  | Stmts v1 -> let v1 = list stmt v1 in G.S (G.Block v1)
  | Program v1 -> let v1 = program v1 in G.Pr v1
  | DictElem v1 -> let v1 = dictorset_elt v1 in G.E v1
      

