(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2011-2015 Tomohiro Matsuyama
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Abstract Syntax Tree for Python.
 *
 * Most of the code in this file derives from code from 
 * Tomohiro Matsuyama in ocaml-pythonlib, which itself derives from
 * the official grammar definition of Python.
 *
 * reference: http://docs.python.org/3/library/ast.html 
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
 * history:
 *  - 2019 port to the pfff infrastructure.
 *  - 2019 modified to support types, and many other Python 3 features
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

(* Contains among other things the position of the token through
 * the Parse_info.token_location embedded inside it, as well as the
 * transformation field that makes possible spatch on the code.
 *)
type tok = Parse_info.info
 (* with tarzan *)

(* a shortcut to annotate some information with token/position information *)
type 'a wrap = 'a * tok
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)
type name = string wrap
 (* with tarzan *)

type dotted_name = name list
 (* with tarzan *)

type resolved_name =
  (* this can be computed by a visitor *)
  | LocalVar
  | Parameter
  | GlobalVar
  | ClassField
  | ImportedModule
  | ImportedEntity

  (* default case *)
  | NotResolved
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
type expr =
  | Num of number (* n *)
  | Str of (string wrap) list (* s *)

  | Name of name (* id *) * expr_context (* ctx *) *
     type_ option * resolved_name ref

  | Tuple of expr list (* elts *)  * expr_context (* ctx *)
  | List of expr list (* elts *)   * expr_context (* ctx *)
  | DictOrSet of dictorset_elt list

  | BoolOp of boolop (* op *) * expr list (* values *)
  | BinOp of expr (* left *) * operator (* op *) * expr (* right *)
  | UnaryOp of unaryop (* op *) * expr (* operand *)
  | Compare of expr (* left *) * cmpop list (* ops *) * expr list (* comparators *)

  | Call of expr (* func *) * argument list (* args *)

  | Subscript of expr (* value *) * slice list (* slice *) * 
                 expr_context (* ctx *)

  | Lambda of parameters (* args *) * expr (* body *)

  | IfExp of expr (* test *) * expr (* body *) * expr (* orelse *)

  | ListComp     of expr (* elt *) * comprehension list (* generators *)
  | GeneratorExp of expr (* elt *) * comprehension list (* generators *)

  | Yield of expr option (* value *)

  | Repr of expr (* value *)
  (* =~ ObjAccess *)
  | Attribute of expr (* value *) * name (* attr *) * expr_context (* ctx *)

  and number =
    | Int of int wrap
    | LongInt of int wrap
    | Float of float wrap
    | Imag of string wrap
  
  and boolop = And | Or
  
  and operator = 
    | Add | Sub | Mult | Div 
    | Mod | Pow | FloorDiv
    | LShift | RShift 
    | BitOr | BitXor | BitAnd 
  
  and unaryop = Invert | Not | UAdd | USub
  
  and cmpop = 
    | Eq | NotEq 
    | Lt | LtE | Gt | GtE 
    | Is | IsNot 
    | In | NotIn
  
  and comprehension =
    expr (* target *) * 
    expr (* iter *) * 
    expr list (* ifs *)
  
  and dictorset_elt = 
    | KeyVal of expr * expr
    | Key of expr
    | PowInline of expr
  
  (* AugLoad and AugStore are not used *)
  and expr_context = 
    | Load | Store 
    | Del 
    | AugLoad | AugStore
    | Param
  
  and slice =
    | Ellipsis
    | Slice of expr option (* lower *) * expr option (* upper *) * expr option (* step *)
    | Index of expr (* value *)
  
  and parameters = 
      (* the first expr can be only a Name or a Tuple (pattern?) *)
      (expr * expr option (* default value *)) list (* args *) * 
      (name * type_ option) option (* varargs *) * 
      (name * type_ option) option (* kwargs *)
  
  and argument = 
    | Arg of expr
    | ArgKwd of name (* arg *) * expr (* value *)
    | ArgStar of expr
    | ArgPow of expr

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)
(* see https://docs.python.org/3/library/typing.html for the semantic
 * and https://www.python.org/dev/peps/pep-3107/ (function annotations)
 * for https://www.python.org/dev/peps/pep-0526/ (variable annotations)
 * for its syntax.
 *)
and type_ = expr
  (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
type stmt =
  | FunctionDef of 
       name (* name *) * 
       parameters (* args *) * 
       type_ option * (* return type *)
       stmt list (* body *) * 
       decorator list (* decorator_list *)

  | ClassDef of 
        name (* name *) * 
        type_ list (* bases *) * 
        stmt list (* body *) * 
        decorator list (* decorator_list *)

  | Assign of expr list (* targets *) * expr (* value *)
  | AugAssign of expr (* target *) * operator (* op *) * expr (* value *)

  | Return of expr option (* value *)

  | Delete of expr list (* targets *)

  | Print of expr option (* dest *) * expr list (* values *) * bool (* nl *)

  | For of expr (* target (pattern) *) * expr (* 'in' iter *) * 
           stmt list (* body *) * stmt list (* orelse *)
  | While of expr (* test *) * stmt list (* body *) * stmt list (* orelse *)
  | If of expr (* test *) * stmt list (* body *) * stmt list (* orelse *)
  | With of expr (* context_expr *) * expr option (* optional_vars *) * stmt list (* body *)

  | Raise of expr option (* type *) * expr option (* inst *) * expr option (* tback *)
  | TryExcept of stmt list (* body *) * excepthandler list (* handlers *) * stmt list (* orelse *)
  | TryFinally of stmt list (* body *) * stmt list (* finalbody *)
  | Assert of expr (* test *) * expr option (* msg *)

  | Import of alias_dotted list (* names *)
  | ImportFrom of dotted_name (* module *) * alias list (* names *) * int option (* level *)

  | Global of name list (* names *)
  | ExprStmt of expr (* value *)

  | Pass
  | Break
  | Continue

and excepthandler = 
  ExceptHandler of 
    type_ option (* type *) * 
    expr option (* name *) * 
    stmt list (* body *)


(* ------------------------------------------------------------------------- *)
(* Decorators (a.k.a annotations) *)
(* ------------------------------------------------------------------------- *)
and decorator = expr

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
and alias_dotted = dotted_name (* name *) * name option (* asname *)

(* ------------------------------------------------------------------------- *)
(* Toplevel *)
(* ------------------------------------------------------------------------- *)
type program = stmt list
  (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Any *)
(* ------------------------------------------------------------------------- *)
type any =
  | Expr of expr
  | Stmt of stmt
  | Stmts of stmt list
  | Program of program
 (* with tarzan *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let str_of_name = fst

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)
let context_of_expr = function
  | Attribute (_, _, ctx) -> Some ctx
  | Subscript (_, _, ctx) -> Some ctx
  | Name (_, ctx, _, _)   -> Some ctx
  | List (_, ctx)         -> Some ctx
  | Tuple (_, ctx)        -> Some ctx
  | _                     -> None
