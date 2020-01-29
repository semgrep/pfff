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

open Ast_go
module G = Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_go to Ast_generic.
 *
 * See ast_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id = fun x -> x
let string = id
let list = List.map
let option = Common.map_opt
let either = Ocaml.map_of_either

let arithmetic_operator = id
let incr_decr = id
let prefix_postfix = id

let error = Ast_generic.error

let name_of_qualified_ident = function
  | Left id -> id, G.empty_name_info
  | Right (xs, id) -> id, { G.name_qualifier = Some xs; name_typeargs = None }

let fake_info () = Parse_info.fake_info "FAKE"

let ii_of_any = Lib_parsing_go.ii_of_any

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tok v = v

let wrap _of_a (v1, v2) = 
  let v1 = _of_a v1 and v2 = tok v2 in 
  (v1, v2)

let ident v = wrap string v

let qualified_ident v = 
  match list ident v with
  | [x] -> Left x
  | [x;y] -> Right ([x], y)
  | _ -> raise Impossible


let rec type_ =
  function
  | TName v1 -> let v1 = qualified_ident v1 in
      G.TyApply (name_of_qualified_ident v1, [])
  | TPtr v1 -> let v1 = type_ v1 in 
      G.TyPointer v1
  | TArray ((v1, v2)) -> let v1 = expr v1 and v2 = type_ v2 in 
      G.TyArray (Some v1, v2)
  | TSlice v1 -> let v1 = type_ v1 in 
      G.TyArray (None, v1) (* not sure worth introducing an OT_Slice *)
  | TArrayEllipsis ((v1, v2)) -> let v1 = tok v1 and v2 = type_ v2 in
      G.TyArray (None, v2)
  | TFunc v1 -> let (params, res) = func_type v1 in 
      G.TyFun (params, res)
  | TMap ((v1, v2)) -> let v1 = type_ v1 and v2 = type_ v2 in 
      let map_name = ("map", fake_info ()), G.empty_name_info in
      G.TyApply (map_name, [G.TypeArg v1; G.TypeArg v2])
  | TChan ((v1, v2)) -> let v1 = chan_dir v1 and v2 = type_ v2 in 
      raise Todo
  | TStruct v1 -> let v1 = list struct_field v1 in 
      raise Todo
  | TInterface v1 -> let v1 = list interface_field v1 in 
      raise Todo

and chan_dir = function | TSend -> () | TRecv -> () | TBidirectional -> ()

and func_type { fparams = fparams; fresults = fresults } =
  let fparams = list parameter fparams in
  let fresults = list parameter fresults in
  raise Todo

and parameter { pname = pname; ptype = ptype; pdots = pdots } =
  let arg = option ident pname in
  let arg = type_ ptype in let arg = option tok pdots in 
  raise Todo

and struct_field (v1, v2) =
  let v1 = struct_field_kind v1 and v2 = option tag v2 in ()

and struct_field_kind =
  function
  | Field ((v1, v2)) -> let v1 = ident v1 and v2 = type_ v2 in ()
  | EmbeddedField ((v1, v2)) ->
      let v1 = option tok v1 and v2 = qualified_ident v2 in ()

and tag v = wrap string v

and interface_field =
  function
  | Method ((v1, v2)) -> let v1 = ident v1 and v2 = func_type v2 in ()
  | EmbeddedInterface v1 -> let v1 = qualified_ident v1 in ()

and expr_or_type v = either expr type_ v



and expr =
  function
  | BasicLit v1 -> let v1 = literal v1 in 
      G.L v1
  | Id (v1, vref) -> let v1 = ident v1 in 
      G.Name ((v1, G.empty_name_info), 
        { G.id_resolved = vref; id_type = ref None })
  | Selector ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = tok v2 and v3 = ident v3 in
      G.ObjAccess (v1, v3)
  | Index ((v1, v2)) -> let v1 = expr v1 and v2 = index v2 in
      G.ArrayAccess (v1, v2)
  | Call v1 -> let (e, args) = call_expr v1 in 
      G.Call (e, args)
  | Cast ((v1, v2)) -> let v1 = type_ v1 and v2 = expr v2 in
      G.Cast (v1, v2)
  | Deref ((v1, v2)) -> let v1 = tok v1 and v2 = expr v2 in
      G.DeRef v2
  | Ref ((v1, v2)) -> let v1 = tok v1 and v2 = expr v2 in
      G.Ref v2
  | Unary ((v1, v2)) ->
      let (v1, tok) = wrap arithmetic_operator v1
      and v2 = expr v2
      in
      G.Call (G.IdSpecial (ArithOp v1, tok), [G.expr_to_arg v2])
  | Binary ((v1, v2, v3)) ->
      let v1 = expr v1
      and (v2, tok) = wrap arithmetic_operator v2
      and v3 = expr v3
      in
      G.Call (G.IdSpecial (ArithOp v2, tok), [v1;v3] |> List.map G.expr_to_arg)
  | CompositeLit ((v1, v2)) ->
      let v1 = type_ v1 and v2 = list init v2 in
      raise Todo
  | Slice ((v1, v2)) ->
      let v1 = expr v1
      and v2 =
        (match v2 with
         | (v1, v2, v3) ->
             let v1 = option expr v1
             and v2 = option expr v2
             and v3 = option expr v3
             in ())
      in 
      raise Todo
  | TypeAssert ((v1, v2)) -> let v1 = expr v1 and v2 = type_ v2 in
      raise Todo
  | EllipsisTODO v1 -> let v1 = tok v1 in 
      raise Todo
  | FuncLit ((v1, v2)) -> let v1 = func_type v1 and v2 = stmt v2 in
      raise Todo
  | Receive ((v1, v2)) -> let v1 = tok v1 and v2 = expr v2 in 
      raise Todo
  | Send ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = tok v2 and v3 = expr v3 in
      raise Todo
  | TypeSwitchExpr ((v1, v2)) -> let v1 = expr v1 and v2 = tok v2 in
      error v2 "TypeSwitchExpr should be handled in Switch statement"
  | ParenType v1 -> let _v1 = type_ v1 in
      error (ii_of_any (T v1) |> List.hd) "ParenType should disappear"

and literal =
  function
  | Int v1 -> let v1 = wrap string v1 in G.Int v1
  | Float v1 -> let v1 = wrap string v1 in G.Float v1
  | Imag v1 -> let v1 = wrap string v1 in G.Imag v1
  | Rune v1 -> let v1 = wrap string v1 in G.Char v1
  | String v1 -> let v1 = wrap string v1 in G.String v1

and index v = expr v

and arguments v = list argument v
and argument =
  function
  | Arg v1 -> let v1 = expr v1 in G.Arg v1
  | ArgType v1 -> let v1 = type_ v1 in G.ArgType v1
  | ArgDots (v1, v2) -> let v1 = expr v1 in let v2 = tok v2 in
      let special = G.Call (G.IdSpecial (G.Spread, v2), [G.expr_to_arg v1]) in
      G.Arg special

and init =
  function
  | InitExpr v1 -> let v1 = expr v1 in v1
  | InitKeyValue ((v1, v2, v3)) ->
      let v1 = init v1 and v2 = tok v2 and v3 = init v3 in
      G.Tuple [v1; v3]
  | InitBraces v1 -> let v1 = list init v1 in
      G.Container (G.List, v1)

and constant_expr v = expr v


and stmt =
  function
  | DeclStmts v1 -> let v1 = list decl v1 in ()
  | Block v1 -> let v1 = list stmt v1 in ()
  | Empty -> ()
  | ExprStmt v1 -> let v1 = expr v1 in ()
  | Assign ((v1, v2, v3)) ->
      let v1 = list expr v1
      and v2 = tok v2
      and v3 = list expr v3
      in ()
  | DShortVars ((v1, v2, v3)) ->
      let v1 = list expr v1
      and v2 = tok v2
      and v3 = list expr v3
      in ()
  | AssignOp ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = wrap arithmetic_operator v2
      and v3 = expr v3
      in ()
  | IncDec ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = wrap incr_decr v2
      and v3 = prefix_postfix v3
      in ()
  | If ((v1, v2, v3, v4)) ->
      let v1 = option stmt v1
      and v2 = expr v2
      and v3 = stmt v3
      and v4 = option stmt v4
      in ()
  | Switch ((v1, v2, v3)) ->
      let v1 = option stmt v1
      and v2 = option stmt v2
      and v3 = list case_clause v3
      in ()
  | Select ((v1, v2)) ->
      let v1 = tok v1 and v2 = list comm_clause v2 in ()
  | For ((v1, v2)) ->
      let v1 =
        (match v1 with
         | (v1, v2, v3) ->
             let v1 = option stmt v1
             and v2 = option expr v2
             and v3 = option stmt v3
             in ())
      and v2 = stmt v2
      in ()
  | Range ((v1, v2, v3, v4)) ->
      let v1 =
        option
          (fun (v1, v2) -> let v1 = list expr v1 and v2 = tok v2 in ())
          v1
      and v2 = tok v2
      and v3 = expr v3
      and v4 = stmt v4
      in ()
  | Return ((v1, v2)) ->
      let v1 = tok v1 and v2 = option (list expr) v2 in ()
  | Break ((v1, v2)) -> let v1 = tok v1 and v2 = option ident v2 in ()
  | Continue ((v1, v2)) ->
      let v1 = tok v1 and v2 = option ident v2 in ()
  | Goto ((v1, v2)) -> let v1 = tok v1 and v2 = ident v2 in ()
  | Fallthrough v1 -> let v1 = tok v1 in ()
  | Label ((v1, v2)) -> let v1 = ident v1 and v2 = stmt v2 in ()
  | Go ((v1, v2)) -> let v1 = tok v1 and v2 = call_expr v2 in ()
  | Defer ((v1, v2)) -> let v1 = tok v1 and v2 = call_expr v2 in ()

and case_clause (v1, v2) = let v1 = case_kind v1 and v2 = stmt v2 in ()
and case_kind =
  function
  | CaseExprs v1 -> let v1 = list expr_or_type v1 in ()
  | CaseAssign ((v1, v2, v3)) ->
      let v1 = list expr_or_type v1
      and v2 = tok v2
      and v3 = expr v3
      in ()
  | CaseDefault v1 -> let v1 = tok v1 in ()

and comm_clause v = case_clause v

and call_expr (v1, v2) = 
  let v1 = expr v1 and v2 = arguments v2 in 
  v1, v2


and decl =
  function
  | DConst ((v1, v2, v3)) ->
      let v1 = ident v1
      and v2 = option type_ v2
      and v3 = option constant_expr v3
      in ()
  | DVar ((v1, v2, v3)) ->
      let v1 = ident v1
      and v2 = option type_ v2
      and v3 = option expr v3
      in ()
  | DTypeAlias ((v1, v2, v3)) ->
      let v1 = ident v1 and v2 = tok v2 and v3 = type_ v3 in ()
  | DTypeDef ((v1, v2)) -> let v1 = ident v1 and v2 = type_ v2 in ()

let top_decl =
  function
  | DFunc ((v1, (v2, v3))) ->
      let v1 = ident v1 and v2 = func_type v2 and v3 = stmt v3 in ()
  | DMethod ((v1, v2, (v3, v4))) ->
      let v1 = ident v1
      and v2 = parameter v2
      and v3 = func_type v3
      and v4 = stmt v4
      in ()
  | D v1 -> let v1 = decl v1 in ()

let rec import { i_path = i_path; i_kind = i_kind } =
  let arg = wrap string i_path in
  let arg = import_kind i_kind in ()
  
and import_kind =
  function
  | ImportOrig -> ()
  | ImportNamed v1 -> let v1 = ident v1 in ()
  | ImportDot v1 -> let v1 = tok v1 in ()

let program2 { package = package; imports = imports; decls = decls } =
  let arg = ident package in
  let arg = list import imports in
  let arg = list top_decl decls in ()
  

let any2 =
  function
  | E v1 -> let v1 = expr v1 in ()
  | S v1 -> let v1 = stmt v1 in ()
  | T v1 -> let v1 = type_ v1 in ()
  | Decl v1 -> let v1 = decl v1 in ()
  | I v1 -> let v1 = import v1 in ()
  | P v1 -> let v1 = program2 v1 in ()
  | Ident v1 -> let v1 = ident v1 in ()
  | Ss v1 -> let v1 = list stmt v1 in ()

let program _ = raise Common.Todo
let any _ = raise Common.Todo
