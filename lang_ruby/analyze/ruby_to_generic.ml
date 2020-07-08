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

open Ast_ruby
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_ruby to AST_generic.
 *
 * See AST_generic.ml for more information.
 *
 * alternatives:
 *  - starting from il_ruby.ml, which is good to get real stmts instead
 *    of stmt_as_expr, but expr may be too far from original expr
 *  - start from an ast_ruby_stmt.ml which is half between ast_ruby.ml and
 *    il_ruby.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id = fun x -> x
let _option = Common.map_opt
let list = List.map

let bool = id
let string = id

let _error = AST_generic.error
let _fake s = Parse_info.fake_info s
let fb = G.fake_bracket

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x
let tok v = info v

let wrap = fun _of_a (v1, v2) ->
  let v1 = _of_a v1 and v2 = info v2 in 
  (v1, v2)

let bracket of_a (t1, x, t2) = (info t1, of_a x, info t2)

let ident x = wrap string x

let rec expr = function
  | Literal x -> literal x
  | Id (id, _kind) -> G.Id (ident id, G.empty_id_info())
  | ScopedId _x -> raise Todo
  | Hash (_bool, xs) -> G.Container (G.Dict, bracket (list expr) xs)
  | Array (xs) -> G.Container (G.Array, bracket (list expr) xs)      
  | Tuple xs -> G.Tuple (list expr xs)
  | Unary (op, e) -> 
    let e = expr e in
    unary op e
  | Binop (e1, op, e2) -> 
      let e1 = expr e1 in
      let e2 = expr e2 in
      binary op e1 e2
  | _ ->  raise Todo

and binary (op, t) e1 e2 =
  match op with
  | B msg ->
      let op = 
        match msg with
        | Op_PLUS ->   Left G.Plus
        | Op_MINUS ->  Left G.Minus
        | Op_TIMES ->  Left G.Mult
        | Op_REM ->    Left G.Mod
        | Op_DIV ->    Left G.Div
        | Op_LSHIFT -> Left G.LSL
        | Op_RSHIFT -> Left G.LSR
        | Op_BAND ->   Left G.BitAnd
        | Op_BOR ->    Left G.BitOr
        | Op_XOR ->    Left G.BitXor
        | Op_POW ->    Left G.Pow
        | Op_CMP ->    Left G.Cmp
        | Op_EQ ->     Left G.Eq
        | Op_EQQ ->    Left G.PhysEq (* abuse PhysEq here, maybe not semantic*)
        | Op_NEQ ->    Left G.NotEq
        | Op_GEQ ->    Left G.GtE
        | Op_LEQ ->    Left G.LtE
        | Op_LT ->     Left G.Lt
        | Op_GT ->     Left G.Gt
        | Op_MATCH ->  raise Todo
        | _ -> raise Todo
      in 
      (match op with
      | Left op ->
         G.Call (G.IdSpecial (G.Op op, t), fb [G.Arg e1; G.Arg e2])
      | Right x -> x
      )
  | _ -> raise Todo

and unary (op,t) e = 
  match op with
  | U msg -> 
      let op = 
        match msg with
        | Op_UMinus -> G.Minus
        | Op_UPlus -> G.Plus
        | Op_UBang -> G.Not
        | Op_UTilde -> G.BitNot
      in
      G.Call (G.IdSpecial (G.Op op, t), fb [G.Arg e])
   | Op_UNot -> G.Call (G.IdSpecial (G.Op G.Not, t), fb [G.Arg e])
   | Op_DefinedQuestion -> G.Call (G.IdSpecial (G.Defined, t), fb [G.Arg e])
   | Op_UStarStar -> G.Call (G.IdSpecial (G.HashSplat, t), fb [G.Arg e])
   (* should be only in arguments, to pass procs. I abuse Ref for now *)
   | Op_UAmper -> G.Ref (t, e)


and literal = function
  | Bool x -> G.L (G.Bool (wrap bool x))
  | Num x -> G.L (G.Int (wrap string x))
  | Float x -> G.L (G.Float (wrap string x))
  | Complex x -> G.L (G.Imag (wrap string x))
  | Rational (x, _tTODOadd_info_in_x) -> G.L (G.Ratio (wrap string x))
  | Char x -> G.L (G.Char (wrap string x))
  | Nil t -> G.L (G.Null (tok t))
  | String _
  | Regexp _
  | Atom _
     -> raise Todo

and expr_as_stmt = function
  | S x -> stmt x
  | D x -> definition x
  | _ -> raise Todo

and stmt = function
  | _ -> raise Todo

and definition = function
  | _ -> raise Todo

let stmts xs = 
  list expr_as_stmt xs

let  program xs = 
  stmts xs



let any = function
  | E x -> 
      (match x with
      | S x -> G.S (stmt x)
      | D x -> G.S (definition x)
      | _ -> G.E (expr x)
      )
  | Pr xs -> G.Ss (stmts xs)