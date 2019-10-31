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

module G = Ast_generic

open Cst_cpp
open Ast_c

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_c to Ast_generic.
 *
 * See ast_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id = fun x -> x
let option = Common.map_opt
let list = List.map
let either f g x = 
  match x with 
  | Left x -> Left (f x) 
  | Right x -> Right (g x)

let string = id

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x

let wrap = fun _of_a (v1, v2) ->
  let v1 = _of_a v1 and v2 = info v2 in 
  (v1, v2)

let name v = wrap string v


let rec unaryOp =
  function
  | GetRef -> (fun e -> G.Ref e)
  | DeRef -> (fun e -> G.DeRef e)
  | UnPlus -> (fun e -> G.Call (G.IdSpecial (G.ArithOp G.Plus), [G.Arg e]))
  | UnMinus -> (fun e -> G.Call (G.IdSpecial (G.ArithOp G.Minus), [G.Arg e]))
  | Tilde -> (fun e -> G.Call (G.IdSpecial (G.ArithOp G.BitNot), [G.Arg e]))
  | Not ->  (fun e -> G.Call (G.IdSpecial (G.ArithOp G.Not), [G.Arg e]))
  | GetRefLabel -> (fun e -> G.OtherExpr (G.OE_GetRefLabel, [G.E e]))
and assignOp =
  function 
  | SimpleAssign -> None
  | OpAssign v1 -> let v1 = arithOp v1 in Some v1

and fixOp = function | Dec -> G.Decr | Inc -> G.Incr
and binaryOp =
  function
  | Arith v1 -> let v1 = arithOp v1 in v1
  | Logical v1 -> let v1 = logicalOp v1 in v1
and arithOp =
  function
  | Plus -> G.Plus
  | Minus -> G.Minus
  | Mul -> G.Mult
  | Div -> G.Div
  | Mod -> G.Mod
  | DecLeft -> G.LSL
  | DecRight -> G.LSR
  | And -> G.BitAnd
  | Or -> G.BitOr
  | Xor -> G.BitXor
and logicalOp =
  function
  | Inf -> G.Lt
  | Sup -> G.Gt
  | InfEq -> G.LtE
  | SupEq -> G.GtE
  | Eq -> G.Eq
  | NotEq -> G.NotEq

  | AndLog -> G.And
  | OrLog -> G.Or


let rec type_ =
  function
  | TBase v1 -> let v1 = name v1 in G.TyBuiltin v1
  | TPointer v1 -> let v1 = type_ v1 in G.TyPointer v1
  | TArray ((v1, v2)) ->
      let v1 = option const_expr v1 and v2 = type_ v2 in
      G.TyArray (v1, v2)
  | TFunction v1 -> let (ret, params) = function_type v1 in 
      (* dropping the optional name *)
      let params = params |> List.map fst in
      G.TyFun (params, ret)
  | TStructName ((v1, v2)) ->
      let v1 = struct_kind v1 and v2 = name v2 in
      G.OtherType (v1, [G.Id v2])
  | TEnumName v1 -> let v1 = name v1 in
      G.OtherType (G.OT_EnumName, [G.Id v1])
  | TTypeName v1 -> 
      let v1 = name v1 in 
      G.TyApply ((v1, G.empty_info()), [])

and function_type (v1, v2) =
  let v1 = type_ v1 and v2 = list parameter v2 in 
  v1, v2

and parameter { p_type = p_type; p_name = p_name } =
  let arg1 = type_ p_type in 
  let arg2 = option name p_name in 
  (arg1, arg2)
and struct_kind = function 
  | Struct -> G.OT_StructName
  | Union -> G.OT_UnionName


and expr =
  function
  | Int v1 -> let v1 = wrap string v1 in G.L (G.Int v1)
  | Float v1 -> let v1 = wrap string v1 in G.L (G.Float v1)
  | String v1 -> let v1 = wrap string v1 in G.L (G.String v1)
  | Char v1 -> let v1 = wrap string v1 in G.L (G.Char v1)

  | Id v1 -> let v1 = name v1 in G.Name (v1, G.empty_info())
  | Ellipses v1 -> let v1 = info v1 in G.Ellipses (v1)
  | Call ((v1, v2)) -> let v1 = expr v1 and v2 = list argument v2 in
      G.Call (v1, v2)
  | Assign ((v1, v2, v3)) ->
      let v1 = wrap assignOp v1
      and v2 = expr v2
      and v3 = expr v3
      in
      (match v1 with
      | None, _ -> G.Assign (v2, v3)
      | Some op, _ -> G.AssignOp (v2, op, v3)
      )
  | ArrayAccess ((v1, v2)) -> let v1 = expr v1 and v2 = expr v2 in
      G.ArrayAccess (v1, v2) 
  | RecordPtAccess ((v1, v2)) -> let v1 = expr v1 and v2 = name v2 in
      G.ObjAccess (G.DeRef v1, v2)
  | Cast ((v1, v2)) -> let v1 = type_ v1 and v2 = expr v2 in
      G.Cast (v1, v2)
  | Postfix ((v1, v2)) ->
      let v1 = expr v1 and v2 = wrap fixOp v2 in 
      G.Call (G.IdSpecial (G.IncrDecr (fst v2, G.Postfix)), [G.Arg v1]) 
  | Infix ((v1, v2)) ->
      let v1 = expr v1 and v2 = wrap fixOp v2 in
      G.Call (G.IdSpecial (G.IncrDecr (fst v2, G.Prefix)), [G.Arg v1]) 
  | Unary ((v1, v2)) ->
      let v1 = expr v1 and v2 = wrap unaryOp v2 in 
      (fst v2) v1
  | Binary ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = wrap binaryOp v2
      and v3 = expr v3
      in G.Call (G.IdSpecial (G.ArithOp (fst v2)), [G.Arg v1; G.Arg v3])
  | CondExpr ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in
      G.Conditional (v1, v2, v3)
  | Sequence ((v1, v2)) -> let v1 = expr v1 and v2 = expr v2 in
      G.Seq [v1;v2]
  | SizeOf v1 -> let v1 = either expr type_ v1 in
      G.Call (G.IdSpecial G.Sizeof, 
       (match v1 with
       | Left e -> [G.Arg e]
       | Right t -> [G.ArgType t]
       ))
  | ArrayInit v1 ->
      let v1 =
        list
          (fun (v1, v2) ->
             let v1 = option expr v1 and v2 = expr v2 in
             (match v1 with
             | None -> v2
             | Some e ->
                  G.OtherExpr (G.OE_ArrayInitDesignator, [G.E e; G.E v2])
            )
        )
          v1
      in G.Container (G.Array, v1)
  | RecordInit v1 ->
      let v1 =
        list (fun (v1, v2) -> let v1 = name v1 and v2 = expr v2 in 
            let entity = G.basic_entity v1 [] in
            let vdef = { G.vinit = Some v2; vtype = None } in
            G.FieldVar (entity, vdef)
        )
          v1
      in G.Record v1
  | GccConstructor ((v1, v2)) -> let v1 = type_ v1 and v2 = expr v2 in
      G.OtherExpr (G.OE_GccConstructor, [G.T v1; G.E v2])

and argument v = 
  let v = expr v in
  G.Arg v

and const_expr v = 
  expr v
  
let rec stmt =
  function
  | ExprSt v1 -> let v1 = expr v1 in G.ExprStmt v1
  | Block v1 -> let v1 = list stmt v1 in G.Block v1
  | If ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = stmt v2 and v3 = stmt v3 in
      G.If (v1, v2, v3)
  | Switch ((v1, v2)) -> let v1 = expr v1 and v2 = list case v2 in
      G.Switch (v1, v2)
  | While ((v1, v2)) -> let v1 = expr v1 and v2 = stmt v2 in
      G.While (v1, v2)
  | DoWhile ((v1, v2)) -> let v1 = stmt v1 and v2 = expr v2 in 
      G.DoWhile (v1, v2)
  | For ((v1, v2, v3, v4)) ->
      let v1 = option expr v1
      and v2 = option expr v2
      and v3 = option expr v3
      and v4 = stmt v4
      in
      let header = 
        G.ForClassic ([G.ForInitExpr (G.opt_to_nop v1)],
          G.opt_to_nop v2,
          G.opt_to_nop v3) in
      G.For (header, v4)
  | Return v1 -> let v1 = option expr v1 in G.Return (G.opt_to_nop v1)
  | Continue -> G.Continue None
  | Break -> G.Break None
  | Label ((v1, v2)) -> let v1 = name v1 and v2 = stmt v2 in
      G.Label (v1, v2)
  | Goto v1 -> let v1 = name v1 in G.Goto v1
  | Vars v1 -> let v1 = list var_decl v1 in
      G.stmt1 (v1 |> List.map (fun v -> G.LocalDef v))
  | Asm v1 -> let v1 = list expr v1 in 
      G.OtherStmt (G.OS_Asm, v1 |> List.map (fun e -> G.E e))

and case =
  function
  | Case ((v1, v2)) -> let v1 = expr v1 and v2 = list stmt v2 in 
      [G.Case v1], G.stmt1 v2
  | Default v1 -> let v1 = list stmt v1 in 
      [G.Default], G.stmt1 v1
and
  var_decl {
               v_name = xname;
               v_type = xtype;
               v_storage = xstorage;
               v_init = init
             } =
  let v1 = name xname in
  let v2 = type_ xtype in
  let v3 = storage xstorage in
  let v4 = option initialiser init in 
  let entity = G.basic_entity v1 v3 in
  entity, G.VarDef {G.vinit = v4; vtype = Some v2}

and initialiser v = expr v

and storage = function 
  | Extern -> [G.Extern] 
  | Static -> [G.Static]
  | DefaultStorage -> []

let func_def {
                 f_name = f_name;
                 f_type = f_type;
                 f_body = f_body;
                 f_static = f_static
               } =
  let v1 = name f_name in
  let (ret, params) = function_type f_type in
  let v3 = list stmt f_body in 
  let v4 = if f_static then [G.Static] else [] in
  let entity = G.basic_entity v1 v4 in
  entity, G.FuncDef {
    G.fparams = params |> List.map (fun (t, nameopt) ->
        G.ParamClassic {
          (G.basic_param (nameopt |> G.opt_to_name)) with
          G.ptype = Some t;
        }
    );
    frettype = Some ret;
    fbody = G.stmt1 v3;
    }

let rec
  struct_def { s_name = s_name; s_kind = s_kind; s_flds = s_flds } =
  let v1 = name s_name in
  let v3 = list field_def s_flds in 
  let entity = G.basic_entity v1 [] in
  (match s_kind with
  | Struct -> 
        let fields = v3 |> List.map (fun (n, t) -> 
              G.basic_field n (Some t)) in
        entity, G.TypeDef ({ G.tbody = G.AndType fields })
  | Union ->
        let ctors = v3 |> List.map (fun (n, t) -> 
              G.OrUnion (n,t))  in
        entity, G.TypeDef ({ G.tbody = G.OrType ctors })
  )

  
and field_def { fld_name = fld_name; fld_type = fld_type } =
  let v1 = option name fld_name in 
  let v2 = type_ fld_type in
  G.opt_to_name v1, v2
  

let enum_def (v1, v2) =
  let v1 = name v1
  and v2 =
    list
      (fun (v1, v2) ->
         let v1 = name v1 and v2 = option const_expr v2 in v1, v2)
      v2
  in
  let entity = G.basic_entity v1 [] in
  let ors = v2 |> List.map (fun (n, eopt) -> G.OrEnum (n, G.opt_to_nop eopt))
  in
  entity, G.TypeDef ({ G.tbody = G.OrType ors})

let type_def (v1, v2) = let v1 = name v1 and v2 = type_ v2 in
  let entity = G.basic_entity v1 [] in
  entity, G.TypeDef ({ G.tbody = G.AliasType v2 })

let define_body =
  function
  | CppExpr v1 -> let v1 = expr v1 in G.E v1
  | CppStmt v1 -> let v1 = stmt v1 in G.S v1

let toplevel =
  function
  | Include v1 -> let v1 = wrap string v1 in 
      G.IDir (G.ImportAll (G.FileName v1, None))
  | Define ((v1, v2)) -> let v1 = name v1 and v2 = define_body v2 in
      G.IDir (G.OtherDirective (G.OI_Define, [G.Id v1; v2]))
  | Macro ((v1, v2, v3)) ->
      let v1 = name v1
      and v2 = list name v2
      and v3 = define_body v3
      in
      G.IDir (
        G.OtherDirective (G.OI_Macro, 
          [G.Id v1] @  (v2 |> List.map (fun n -> G.Id n)) @ [v3]))
  | StructDef v1 -> let v1 = struct_def v1 in
      G.IDef v1
  | TypeDef v1 -> let v1 = type_def v1 in
      G.IDef v1
  | EnumDef v1 -> let v1 = enum_def v1 in
      G.IDef v1
  | FuncDef v1 -> let v1 = func_def v1 in
      G.IDef v1
  | Global v1 -> let v1 = var_decl v1 in
      G.IDef v1
  | Prototype v1 -> let v1 = func_def v1 in 
      G.IDef v1

let program v = 
 list toplevel v

let any =
  function
  | Expr v1 -> let v1 = expr v1 in G.E v1
  | Stmt v1 -> let v1 = stmt v1 in G.S v1
  | Type v1 -> let v1 = type_ v1 in G.T v1
  | Toplevel v1 -> let v1 = toplevel v1 in G.I v1
  | Program v1 -> let v1 = program v1 in G.Pr v1

