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

module A = Ast_python
module G = Ast_generic

open Ast_python

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

let bool = id
let int = id
let float = id
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
  | Num v1 -> let v1 = number v1 in v1
  | Str ((v1, v2)) -> 
    let v1 = string v1 
    and v2 = list tok v2 in 
    ()

  | Name ((v1, v2, v3, v4)) ->
      let v1 = name v1
      and v2 = expr_context v2
      and v3 = option type_ v3
      and v4 = vref resolved_name v4
      in ()
  | Tuple ((v1, v2)) ->
      let v1 = list expr v1 and v2 = expr_context v2 in ()
  | List ((v1, v2)) ->
      let v1 = list expr v1 and v2 = expr_context v2 in ()
  | Subscript ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = slice v2 and v3 = expr_context v3 in ()
  | Attribute ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = name v2 and v3 = expr_context v3 in ()

  | DictOrSet (v) -> let v = list dictorset_elt v in ()
  | ListComp ((v1, v2)) ->
      let v1 = expr v1 and v2 = list comprehension v2 in ()

  | BoolOp ((v1, v2)) -> let v1 = boolop v1 and v2 = list expr v2 in ()
  | BinOp ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = operator v2 and v3 = expr v3 in ()
  | UnaryOp ((v1, v2)) -> let v1 = unaryop v1 and v2 = expr v2 in ()
  | Compare ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = list cmpop v2
      and v3 = list expr v3
      in ()

  | Call (v1, v2) -> let v1 = expr v1 in let v2 = list argument v2 in ()


  | Lambda ((v1, v2)) -> let v1 = parameters v1 and v2 = expr v2 in ()
  | IfExp ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in ()
  | GeneratorExp ((v1, v2)) ->
      let v1 = expr v1 and v2 = list comprehension v2 in ()
  | Yield v1 -> let v1 = option expr v1 in ()
  | Repr v1 -> let v1 = expr v1 in ()

and argument = function
  | Arg e -> expr e
  | ArgPow e -> expr e
  | ArgStar e -> expr e
  | ArgKwd (n, e) -> let n = name n in let e = expr e in ()

and dictorset_elt = function
  | KeyVal (v1, v2) -> let v1 = expr v1 in let v2 =  expr v2 in ()
  | Key (v1) -> expr v1
  | PowInline (v1) -> expr v1
  
and number =
  function
  | Int v1     -> let v1 = wrap int v1 in ()
  | LongInt v1 -> let v1 = wrap int v1 in ()
  | Float v1   -> let v1 = wrap float v1 in ()
  | Imag v1    -> let v1 = wrap string v1 in ()


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
  let v1 = expr v1 and v2 = expr v2 and v3 = list expr v3 in ()

and keyword (v1, v2) = let v1 = name v1 and v2 = expr v2 in ()

and slice =
  function
  | Index v1 -> let v1 = expr v1 in ()
  | Ellipsis -> ()
  | Slice ((v1, v2, v3)) ->
      let v1 = option expr v1
      and v2 = option expr v2
      and v3 = option expr v3
      in ()
  | ExtSlice v1 -> let v1 = list slice v1 in ()

and parameters x =
  let (v1, v2, v3, v4) = x in
  let v1 = list expr v1
  and v2 = option name_and_type v2
  and v3 = option name_and_type v3
  and v4 = list expr v4
  in ()

and name_and_type (v1, v2) =
  let v1 = name v1 in
  let v2 = option type_ v2 in
  ()

and type_ v = 
  expr v

and stmt x =
  match x with
  | FunctionDef ((v1, v2, v3, v4, v5)) ->
      let v1 = name v1
      and v2 = parameters v2
      and v3 = option type_ v3
      and v4 = list stmt v4
      and v5 = list decorator v5
      in ()
  | ClassDef ((v1, v2, v3, v4)) ->
      let v1 = name v1
      and v2 = list expr v2
      and v3 = list stmt v3
      and v4 = list decorator v4
      in ()

  | Assign ((v1, v2)) -> let v1 = list expr v1 and v2 = expr v2 in ()
  | AugAssign ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = operator v2 and v3 = expr v3 in ()

  | Return v1 -> let v1 = option expr v1 in ()

  | Delete v1 -> let v1 = list expr v1 in ()
  | Print ((v1, v2, v3)) ->
      let v1 = option expr v1
      and v2 = list expr v2
      and v3 = bool v3
      in ()

  | For ((v1, v2, v3, v4)) ->
      let v1 = expr v1
      and v2 = expr v2
      and v3 = list stmt v3
      and v4 = list stmt v4
      in ()
  | While ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = list stmt v2
      and v3 = list stmt v3
      in ()
  | If ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = list stmt v2
      and v3 = list stmt v3
      in ()

  | With ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = option expr v2
      and v3 = list stmt v3
      in ()

  | Raise ((v1, v2, v3)) ->
      let v1 = option expr v1
      and v2 = option expr v2
      and v3 = option expr v3
      in ()
  | TryExcept ((v1, v2, v3)) ->
      let v1 = list stmt v1
      and v2 = list excepthandler v2
      and v3 = list stmt v3
      in ()
  | TryFinally ((v1, v2)) ->
      let v1 = list stmt v1 and v2 = list stmt v2 in ()

  | Assert ((v1, v2)) -> let v1 = expr v1 and v2 = option expr v2 in ()

  | Import v1 -> let v1 = list alias2 v1 in ()
  | ImportFrom ((v1, v2, v3)) ->
      let v1 = dotted_name v1
      and v2 = list alias v2
      and v3 = option int v3
      in ()

  | Exec ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = option expr v2
      and v3 = option expr v3
      in ()

  | Global v1 -> let v1 = list name v1 in ()

  | ExprStmt v1 -> let v1 = expr v1 in ()

  | Pass -> ()
  | Break -> ()
  | Continue -> ()


and excepthandler =
  function
  | ExceptHandler ((v1, v2, v3)) ->
      let v1 = option type_ v1
      and v2 = option expr v2
      and v3 = list stmt v3
      in ()

and decorator v = expr v

and alias (v1, v2) = let v1 = name v1 and v2 = option name v2 in ()
and alias2 (v1, v2) = let v1 = dotted_name v1 and v2 = option name v2 in ()

and modl =
  function
  | Module v1 -> let v1 = list stmt v1 in ()
  | Interactive v1 -> let v1 = list stmt v1 in ()
  | Expression v1 -> let v1 = expr v1 in ()
  | Suite v1 -> let v1 = list stmt v1 in ()
  
let program v = 
  (* modl v *)
  raise Todo

let any =
  function
  | Expr v1 -> let v1 = expr v1 in ()
  | Stmt v1 -> let v1 = stmt v1 in ()
  | Stmts v1 -> let v1 = list stmt v1 in ()
  | Modl v1 -> let v1 = modl v1 in ()
  | Program v1 -> let v1 = program v1 in ()
  
