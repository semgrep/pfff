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

let arithmetic_operator _ = ()
let incr_decr _ = ()
let prefix_postfix _ = ()

let error = Ast_generic.error

(* need visitor_go.ml and lib_parsing_go.ml *)
let ii_of_any x = 
  raise Todo

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tok v = v

let wrap _of_a (v1, v2) = 
  let v1 = _of_a v1 and v2 = tok v2 in 
  (v1, v2)

let ident v = wrap string v

let qualified_ident v = list ident v

let rec type_ =
  function
  | TName v1 -> let v1 = qualified_ident v1 in ()
  | TPtr v1 -> let v1 = type_ v1 in ()
  | TArray ((v1, v2)) -> let v1 = expr v1 and v2 = type_ v2 in ()
  | TSlice v1 -> let v1 = type_ v1 in ()
  | TArrayEllipsis ((v1, v2)) -> let v1 = tok v1 and v2 = type_ v2 in ()
  | TFunc v1 -> let v1 = func_type v1 in ()
  | TMap ((v1, v2)) -> let v1 = type_ v1 and v2 = type_ v2 in ()
  | TChan ((v1, v2)) -> let v1 = chan_dir v1 and v2 = type_ v2 in ()
  | TStruct v1 -> let v1 = list struct_field v1 in ()
  | TInterface v1 -> let v1 = list interface_field v1 in ()

and chan_dir = function | TSend -> () | TRecv -> () | TBidirectional -> ()

and func_type { fparams = fparams; fresults = fresults } =
  let arg = list parameter fparams in
  let arg = list parameter fresults in ()

and parameter { pname = pname; ptype = ptype; pdots = pdots } =
  let arg = option ident pname in
  let arg = type_ ptype in let arg = option tok pdots in ()

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
  | BasicLit v1 -> let v1 = literal v1 in ()
  | CompositeLit ((v1, v2)) ->
      let v1 = type_ v1 and v2 = list init v2 in ()
  | Id (v1, _IGNORED) -> let v1 = ident v1 in ()
  | Selector ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = tok v2 and v3 = ident v3 in ()
  | Index ((v1, v2)) -> let v1 = expr v1 and v2 = index v2 in ()
  | Slice ((v1, v2)) ->
      let v1 = expr v1
      and v2 =
        (match v2 with
         | (v1, v2, v3) ->
             let v1 = option expr v1
             and v2 = option expr v2
             and v3 = option expr v3
             in ())
      in ()
  | Call v1 -> let v1 = call_expr v1 in ()
  | Cast ((v1, v2)) -> let v1 = type_ v1 and v2 = expr v2 in ()
  | Deref ((v1, v2)) -> let v1 = tok v1 and v2 = expr v2 in ()
  | Ref ((v1, v2)) -> let v1 = tok v1 and v2 = expr v2 in ()
  | Receive ((v1, v2)) -> let v1 = tok v1 and v2 = expr v2 in ()
  | Unary ((v1, v2)) ->
      let v1 = wrap arithmetic_operator v1
      and v2 = expr v2
      in ()
  | Binary ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = wrap arithmetic_operator v2
      and v3 = expr v3
      in ()
  | TypeAssert ((v1, v2)) -> let v1 = expr v1 and v2 = type_ v2 in ()
  | TypeSwitchExpr ((v1, v2)) -> let v1 = expr v1 and v2 = tok v2 in ()
  | EllipsisTODO v1 -> let v1 = tok v1 in ()
  | FuncLit ((v1, v2)) -> let v1 = func_type v1 and v2 = stmt v2 in ()
  | ParenType v1 -> let v1 = type_ v1 in ()
  | Send ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = tok v2 and v3 = expr v3 in ()

and literal =
  function
  | Int v1 -> let v1 = wrap string v1 in ()
  | Float v1 -> let v1 = wrap string v1 in ()
  | Imag v1 -> let v1 = wrap string v1 in ()
  | Rune v1 -> let v1 = wrap string v1 in ()
  | String v1 -> let v1 = wrap string v1 in ()

and index v = expr v

and arguments v = list argument v
and argument =
  function
  | Arg v1 -> let v1 = expr v1 in ()
  | ArgType v1 -> let v1 = type_ v1 in ()
  | ArgDots v1 -> let v1 = tok v1 in ()

and init =
  function
  | InitExpr v1 -> let v1 = expr v1 in ()
  | InitKeyValue ((v1, v2, v3)) ->
      let v1 = init v1 and v2 = tok v2 and v3 = init v3 in ()
  | InitBraces v1 -> let v1 = list init v1 in ()

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

and call_expr (v1, v2) = let v1 = expr v1 and v2 = arguments v2 in ()


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
  | DFunc ((v1, v2, v3)) ->
      let v1 = ident v1 and v2 = func_type v2 and v3 = stmt v3 in ()
  | DMethod ((v1, v2, v3, v4)) ->
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
