(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2011-2015 Tomohiro Matsuyama
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Abstract Syntax Tree for Python.
 *
 * Most of the code in this file derives from code from 
 * Tomohiro Matsuyama in ocaml-pythonlib, which itself derives from
 * the official grammar definition of Python.
 *
 * specification: http://docs.python.org/3/library/ast.html 
 *
 * See also:
 *  - http://trevorjim.com/python-is-not-context-free/
 * 
 * related work:
 *  - https://github.com/m2ym/ocaml-pythonlib
 *    The original code. The repo was also forked by jeremy buisson
 *    who added a very basic simplifier but remains mostly the same.
 *  - Pyre-check
 *    typechecker and taint-tracker for Python, written in OCaml from facebook
 *  - https://github.com/mattgreen/hython
 *    Python3 interpreter written in Haskell
 *    
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

(* Contains among other things the position of the token through
 * the Parse_info.token_location embedded inside it, as well as the
 * transformation field that makes possible spatch on Javascript code.
 *)
type tok = Parse_info.info

(* a shortcut to annotate some information with token/position information *)
and 'a wrap = 'a * tok

 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)
type name = string wrap
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)

type expr =
  | Num of number (* n *)
  | Str of string (* s *)

  | Name of name (* id *) * expr_context (* ctx *)

  | Tuple of expr list (* elts *) * expr_context (* ctx *)
  | List of expr list (* elts *) * expr_context (* ctx *)
  | Dict of expr list (* keys *) * expr list (* values *)
  | ListComp of expr (* elt *) * comprehension list (* generators *)

  | BoolOp of boolop (* op *) * expr list (* values *)
  | BinOp of expr (* left *) * operator (* op *) * expr (* right *)
  | UnaryOp of unaryop (* op *) * expr (* operand *)
  | Compare of expr (* left *) * cmpop list (* ops *) * expr list (* comparators *)

  | Call of expr (* func *) * expr list (* args *) * keyword list (* keywords *) * expr option (* starargs *) * expr option (* kwargs *)

  | Subscript of expr (* value *) * slice (* slice *) * expr_context (* ctx *)

  | Lambda of arguments (* args *) * expr (* body *)

  | IfExp of expr (* test *) * expr (* body *) * expr (* orelse *)

  | GeneratorExp of expr (* elt *) * comprehension list (* generators *)
  | Yield of expr option (* value *)

  | Repr of expr (* value *)

  | Attribute of expr (* value *) * name (* attr *) * expr_context (* ctx *)

and number =
  | Int of int
  | LongInt of int
  | Float of float
  | Imag of string

and boolop = And | Or

and operator = Add | Sub | Mult | Div | Mod | Pow | LShift
               | RShift | BitOr | BitXor | BitAnd | FloorDiv

and unaryop = Invert | Not | UAdd | USub

and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn

and comprehension = expr (* target *) * expr (* iter *) * expr list (* ifs *)

(* AugLoad and AugStore are not used *)
and expr_context = Load | Store | Del | AugLoad | AugStore | Param

and keyword = name (* arg *) * expr (* value *)

and slice =
  | Ellipsis
  | Slice of expr option (* lower *) * expr option (* upper *) * expr option (* step *)
  | ExtSlice of slice list (* dims *)
  | Index of expr (* value *)

and arguments = expr list (* args *) * name option (* varargs *) * name option (* kwargs *) * expr list (* defaults *)

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)


type stmt =
  | FunctionDef of name (* name *) * arguments (* args *) * stmt list (* body *) * expr list (* decorator_list *)
  | ClassDef of name (* name *) * expr list (* bases *) * stmt list (* body *) * expr list (* decorator_list *)

  | Return of expr option (* value *)

  | Delete of expr list (* targets *)
  | Assign of expr list (* targets *) * expr (* value *)
  | AugAssign of expr (* target *) * operator (* op *) * expr (* value *)

  | Print of expr option (* dest *) * expr list (* values *) * bool (* nl *)

  | For of expr (* target *) * expr (* iter *) * stmt list (* body *) * stmt list (* orelse *)
  | While of expr (* test *) * stmt list (* body *) * stmt list (* orelse *)
  | If of expr (* test *) * stmt list (* body *) * stmt list (* orelse *)
  | With of expr (* context_expr *) * expr option (* optional_vars *) * stmt list (* body *)

  | Raise of expr option (* type *) * expr option (* inst *) * expr option (* tback *)
  | TryExcept of stmt list (* body *) * excepthandler list (* handlers *) * stmt list (* orelse *)
  | TryFinally of stmt list (* body *) * stmt list (* finalbody *)
  | Assert of expr (* test *) * expr option (* msg *)

  | Import of alias list (* names *)
  | ImportFrom of name (* module *) * alias list (* names *) * int option (* level *)

  | Exec of expr (* body *) * expr option (* globals *) * expr option (* locals *)

  | Global of name list (* names *)
  | Expr of expr (* value *)

  | Pass
  | Break
  | Continue

and excepthandler = ExceptHandler of expr option (* type *) * expr option (* name *) * stmt list (* body *)

(* ------------------------------------------------------------------------- *)
(* Type *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Function (or method) definition *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Variable definition *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Class definition *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Module import/export *)
(* ------------------------------------------------------------------------- *)

and alias = name (* name *) * name option (* asname *)

(* ------------------------------------------------------------------------- *)
(* Toplevel *)
(* ------------------------------------------------------------------------- *)
and modl =
  | Module of stmt list (* body *)
  | Interactive of stmt list (* body *)
  | Expression of expr (* body *)

  | Suite of stmt list (* body *)
  (* with tarzan *)

type program = modl list

(* ------------------------------------------------------------------------- *)
(* Any *)
(* ------------------------------------------------------------------------- *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)


