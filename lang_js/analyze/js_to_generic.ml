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

open Ast_js
module G = Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_js to Ast_generic.
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

let bool = id
let string = id

exception Error of string * Parse_info.info

let error tok msg = 
  raise (Error (msg, tok))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x
let tok v = info v

let wrap = fun _of_a (v1, v2) ->
  let v1 = _of_a v1 and v2 = info v2 in 
  (v1, v2)

let name v = wrap id v

let filename v = wrap string v

let label v = wrap string v

let qualified_name x = [x, Parse_info.fake_info "TODO qualified name"]

let resolved_name = function
  | Local -> G.Local
  | Param -> G.Param
  | Global x -> G.Global (qualified_name x)
  | NotResolved -> G.NotResolved

type special_result = 
  | SR_Special of G.special
  | SR_Other of G.other_expr_operator
  | SR_Literal of G.literal
  | SR_NeedArgs of (G.expr list -> G.expr)

let special (x, tok) = 
  match x with
  | UseStrict -> SR_Other G.OE_UseStrict
  | Null -> SR_Literal (G.Null tok) 
  | Undefined -> SR_Literal (G.Undefined tok)
  | This -> SR_Special G.This
  | Super -> SR_Special G.Super
  | Require -> SR_Other G.OE_Require (* TODO: left up to include? *)
  | Exports -> SR_Other G.OE_Exports
  | Module -> SR_Other G.OE_Module
  | Define -> SR_Other G.OE_Define
  | Arguments -> SR_Other G.OE_Arguments
  | New -> SR_Special G.New
  | NewTarget -> SR_Other G.OE_NewTarget
  | Eval -> SR_Special G.Eval
  | Seq -> SR_NeedArgs (fun args -> G.Seq args)
  | Typeof -> SR_Special G.Typeof
  | Instanceof -> SR_Special G.Instanceof
  | In -> SR_Other G.OE_In
  | Delete -> SR_Other G.OE_Delete
  | Void -> SR_Literal (G.Unit tok)
  | Spread -> SR_Special G.Spread
  | Yield -> SR_NeedArgs (fun args -> 
          match args with
          | [e] -> G.Yield e
          | _ -> error tok "Impossible: Too many arguments to Yield"
          )
  | YieldStar -> SR_Other G.OE_YieldStar
  | Await -> SR_NeedArgs (fun args ->
          match args with
          | [e] -> G.Await e
          | _ -> error tok "Impossible: Too many arguments to Await"
          )
  | Encaps v1 -> 
      (match v1 with
      | None -> SR_NeedArgs (fun args -> 
          G.Call (G.IdSpecial G.Concat, args |> List.map (fun e -> G.Arg e)))
      | Some n -> 
            let n = name n in
            SR_NeedArgs (fun args ->
            G.OtherExpr (G.OE_Encaps,(G.N n)::(args|>List.map(fun e ->G.E e))))
      )
  | Not -> SR_Special (G.ArithOp G.Not)
  | And -> SR_Special (G.ArithOp G.And)
  | Or -> SR_Special (G.ArithOp G.Or)
  | BitNot -> SR_Special (G.ArithOp G.BitNot)
  | BitAnd -> SR_Special (G.ArithOp G.BitAnd)
  | BitOr -> SR_Special (G.ArithOp G.BitOr)
  | BitXor -> SR_Special (G.ArithOp G.BitXor)
  | Lsr -> SR_Special (G.ArithOp G.LSR)
  | Asr -> SR_Special (G.ArithOp G.ASR)
  | Lsl -> SR_Special (G.ArithOp G.LSL)
  | Equal -> SR_Special (G.ArithOp G.Eq)
  | PhysEqual -> SR_Special (G.ArithOp G.PhysEq)
  | Lower -> SR_Special (G.ArithOp G.Lt)
  | Greater -> SR_Special (G.ArithOp G.Gt)
  | Plus -> SR_Special (G.ArithOp G.Plus)
  | Minus -> SR_Special (G.ArithOp G.Minus)
  | Mul -> SR_Special (G.ArithOp G.Mult)
  | Div -> SR_Special (G.ArithOp G.Div)
  | Mod -> SR_Special (G.ArithOp G.Mod)
  | Expo -> SR_Special (G.ArithOp G.Pow)
  | IncrDecr v -> SR_Special (G.IncrDecr v)

let rec property_name =
  function
  | PN v1 -> let v1 = name v1 in Left v1
  | PN_Computed v1 -> let v1 = expr v1 in Right v1

and expr (x: expr) =
  match x with
  | Bool v1 -> let v1 = wrap bool v1 in G.L (G.Bool v1)
  | Num v1 -> let v1 = wrap string v1 in G.L (G.Float v1)
  | String v1 -> let v1 = wrap string v1 in G.L (G.String v1)
  | Regexp v1 -> let v1 = wrap string v1 in G.L (G.Regexp v1)
  | Id (v1, refresolved) -> 
      let v1 = name v1 in
      let v2 = { (G.empty_info ()) with 
                 G.id_resolved = vref resolved_name refresolved } in
      G.Id (v1, v2)

  | IdSpecial v1 -> 
      let x = special v1 in
      (match x with
      | SR_Special v -> G.IdSpecial v
      | SR_NeedArgs _ -> 
          error (snd v1) "Impossible: should have been matched in Call first"
      | SR_Literal l -> G.L l
      | SR_Other x -> G.OtherExpr (x, [])
      )
  | Nop -> G.Nop
  | Assign ((v1, v2)) -> let v1 = expr v1 and v2 = expr v2 in 
      G.Assign (v1, v2)
  | ArrAccess ((v1, v2)) -> let v1 = expr v1 and v2 = expr v2 in 
      G.ArrayAccess (v1, v2)
  | Obj v1 -> let flds = obj_ v1 in G.Record flds
  | Class (v1, _v2TODO) -> 
      let def, _more_attrsTODOEMPTY  = class_ v1 in
      G.OtherExpr (G.OE_ExprClass, [G.Dk (G.ClassDef def)])
  | ObjAccess ((v1, v2)) ->
      let v1 = expr v1 in
      let v2 = property_name v2 in
      (match v2 with
      | Left n -> G.ObjAccess (v1, n)
      | Right e -> G.OtherExpr (G.OE_ObjAccess_PN_Computed, [G.E v1; G.E e])
      )
  | Fun ((v1, _v2TODO)) -> 
      let def, _more_attrs   = fun_ v1 in
      (* todo? assert more_attrs = []? *)
      G.Lambda (def)

  | Apply ((IdSpecial v1, v2)) ->
      let x = special v1 in
      let v2 = list expr v2 in 
      (match x with
      | SR_Special v -> 
        G.Call (G.IdSpecial v, v2 |> List.map (fun e -> G.Arg e))
      | SR_Literal _ ->
        error (snd v1) "Weird: literal in call position"
      | SR_Other x -> (* ex: NewTarget *)
        G.Call (G.OtherExpr (x, []), v2 |> List.map (fun e -> G.Arg e))
      | SR_NeedArgs f ->
        f v2
      )
  | Apply ((v1, v2)) -> let v1 = expr v1 and v2 = list expr v2 in 
      G.Call (v1, v2 |> List.map (fun e -> G.Arg e))
  | Arr ((v1)) -> let v1 = list expr v1 in G.Container (G.Array, v1)
  | Conditional ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in
      G.Conditional (v1, v2, v3)

and stmt x =
  match x with
  | VarDecl v1 -> let v1 = def_of_var v1 in G.LocalDef (v1)
  | Block v1 -> let v1 = list stmt v1 in G.Block v1
  | ExprStmt v1 -> let v1 = expr v1 in G.ExprStmt v1
  | If ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = stmt v2 and v3 = stmt v3 in 
      G.If (v1, v2, v3)
  | Do ((v1, v2)) -> let v1 = stmt v1 and v2 = expr v2 in 
      G.DoWhile (v1, v2)
  | While ((v1, v2)) -> let v1 = expr v1 and v2 = stmt v2 in
      G.While (v1, v2)
  | For ((v1, v2)) -> let v1 = for_header v1 and v2 = stmt v2 in
      G.For (v1, v2)
  | Switch ((v1, v2)) -> let v1 = expr v1 and v2 = list case v2 in
      G.Switch (v1, v2)
  | Continue v1 -> let v1 = option label v1 in 
     G.Continue (v1 |> option (fun n -> G.Id (n, G.empty_info ())))
  | Break v1 -> let v1 = option label v1 in
     G.Break (v1 |> option (fun n -> G.Id (n, G.empty_info ())))
  | Return v1 -> let v1 = expr v1 in G.Return v1
  | Label ((v1, v2)) -> let v1 = label v1 and v2 = stmt v2 in
      G.Label (v1, v2)
  | Throw v1 -> let v1 = expr v1 in G.Throw v1
  | Try ((v1, v2, v3)) ->
      let v1 = stmt v1
      and v2 =
        option (fun (v1, v2) -> 
           let v1 = name v1 and v2 = stmt v2 in
           G.PatVar v1, v2
       ) v2
      and v3 = option stmt v3 in
      G.Try (v1, Common.opt_to_list v2, v3)

and for_header =
  function
  | ForClassic ((v1, v2, v3)) ->
      let v2 = expr v2 in
      let v3 = expr v3 in
      (match v1 with
      | Left vars ->
            let vars = vars |> List.map (fun x -> 
                  let (a,b) = var_of_var x in
                  G.ForInitVar (a, b)
            )
            in
            G.ForClassic (vars, v2, v3)
      | Right e ->
         let e = expr e in
         G.ForClassic ([G.ForInitExpr e], v2, v3)
      )
      
  | ForIn ((v1, v2)) ->
      let v2 = expr v2 in
      let pattern = 
        match v1 with
        | Left v -> 
            let v = def_of_var v in
            G.OtherPat (G.OP_Var, [G.D v])
        | Right e ->
            let e = expr e in
            G.OtherPat (G.OP_Expr, [G.E e])
      in
      G.ForEach (pattern, v2)

and case =
  function
  | Case ((v1, v2)) -> let v1 = expr v1 and v2 = stmt v2 in
      [G.Case v1], v2
  | Default v1 -> let v1 = stmt v1 in
      [G.Default], v1

and def_of_var { v_name = x_name; v_kind = x_kind; 
                 v_init = x_init; v_resolved = x_resolved } =
  let v1 = name x_name in
  let v2 = var_kind x_kind in 
  let ent = G.basic_entity v1 [v2] in
  (match x_init with
  | Fun (v3, _nTODO)   -> 
      let def, more_attrs = fun_ v3 in
      { ent with G.attrs = ent.G.attrs @ more_attrs}, G.FuncDef def
  | Class (v3, _nTODO) -> 
      let def, more_attrs = class_ v3 in
      { ent with G.attrs = ent.G.attrs @ more_attrs}, G.ClassDef def
  | _ -> 
       let v3 = expr x_init in 
       let _v4TODO = vref resolved_name x_resolved in
       ent, G.VarDef { G.vinit = Some v3; G.vtype = None }
   )

and var_of_var { v_name = x_name; v_kind = x_kind; 
                 v_init = x_init; v_resolved = x_resolved } =
  let v1 = name x_name in
  let v2 = var_kind x_kind in 
  let ent = G.basic_entity v1 [v2] in

  let v3 = expr x_init in 
  let _v4TODO = vref resolved_name x_resolved in
  ent, { G.vinit = Some v3; G.vtype = None }


and var_kind = function | Var -> G.Var | Let -> G.Let | Const -> G.Const

and fun_ { f_props = f_props; f_params = f_params; f_body = f_body } =
  let v1 = list fun_prop f_props in
  let v2 = list parameter f_params in 
  let v3 = stmt f_body in
  { G.fparams = v2; frettype = None; fbody = v3; }, v1

and parameter x =
 match x with
 { p_name = p_name; p_default = p_default; p_dots = p_dots } ->
  let v1 = name p_name in
  let v2 = option expr p_default in 
  let v3 = bool p_dots in
  G.ParamClassic { G.pname = v1; pdefault = v2; ptype = None;
        pattrs = if v3 then [G.Variadic] else [];
  }
  

and fun_prop =
  function 
  | Get -> G.Getter | Set -> G.Setter
  | Generator -> G.Generator | Async -> G.Async

and obj_ v = list property v

and class_ { c_extends = c_extends; c_body = c_body } =
  let v1 = option expr c_extends in
  let v2 = list property c_body in 
  (* todo: could analyze arg to look for Id *)
  let extends = 
    match v1 with
    | None -> [] 
    | Some e -> [G.OtherType (G.OT_Expr, [G.E e])]
  in
  { G.ckind = G.Class; cextends = extends; cimplements = []; cbody = v2;}, []
and property x =
   match x with
  | Field ((v1, v2, v3)) ->
      let v1 = property_name v1
      and v2 = list property_prop v2
      and v3 = expr v3
      in 
      (match v1 with
      | Left n ->
        let ent = G.basic_entity n v2 in
       (* todo: could be a Lambda in which case we should return a FuncDef? *)
        G.FieldVar (ent, { G.vinit = Some v3; vtype = None })
      | Right e ->
        G.FieldDynamic (e, v2, v3)
      )
  | FieldSpread v1 -> 
      let v1 = expr v1 in 
      G.FieldSpread v1

and property_prop =
  function 
  | Static -> G.Static 
  | Public -> G.Public | Private -> G.Private | Protected -> G.Protected
  

let rec toplevel x =
  match x with
  | V v1 -> let v1 = def_of_var v1 in G.IDef v1
  | S ((v1, v2)) -> let _v1TODO = tok v1 and v2 = stmt v2 in G.IStmt v2
  | M v1 -> let v1 = module_directive v1 in G.IDir v1


and module_directive x = 
  match x with
  | Import ((v1, v2, v3)) ->
      let v1 = name v1 and v2 = name v2 and v3 = filename v3 in 
      G.Import (G.FileName v3, [v1, Some v2])
  | ModuleAlias ((v1, v2)) ->
      let v1 = name v1 and v2 = filename v2 in
      G.ImportAll (G.FileName v2, Some v1)
  | ImportCss ((v1)) ->
      let v1 = name v1 in
      G.OtherDirective (G.OI_ImportCss, [G.N v1])
  | ImportEffect ((v1)) ->
      let v1 = name v1 in
      G.OtherDirective (G.OI_ImportEffect, [G.N v1])
  | Export ((v1)) -> let v1 = name v1 in
      G.OtherDirective (G.OI_Export, [G.N v1])

and program v = list toplevel v


let any =
  function
  | Expr v1 -> let v1 = expr v1 in G.E v1
  | Stmt v1 -> let v1 = stmt v1 in G.S v1
  | Top v1 -> let v1 = toplevel v1 in G.I v1
  | Program v1 -> let v1 = program v1 in G.Pr v1

