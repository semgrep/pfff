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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Abstract Syntax Tree for Go.
 *
 * This file tries to keep the convention used in the official ast.go
 * implementation (e.g., it uses FuncLit instead of the more common Lambda).
 *
 * reference: https://golang.org/src/go/ast/ast.go
 *)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
(* Contains among other things the position of the token through
 * the Parse_info.token_location embedded inside it, as well as the
 * transformation field that makes possible spatch on the code.
 *)
type tok = Parse_info.t
 (* with tarzan *)

(* a shortcut to annotate some information with token/position information *)
type 'a wrap = 'a * tok
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Ident, qualifier *)
(* ------------------------------------------------------------------------- *)
(* for ?/?/? *)
type ident = string wrap
 (* with tarzan *)

(* for ?/?/?  (called names in ast.go) *)
type qualified_ident = ident list (* 1 or 2 elements *)
 (* with tarzan *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
type type_ =
 | TName of qualified_ident (* included the basic types: bool/int/... *)
 | TPtr of type_

 | TArray of expr * type_
 | TSlice of type_
  (* only in CompositeLit (could be rewritten as TArray with static length) *)
 | TArrayEllipsis of tok (* ... *) * type_ 

 | TFunc of func_type
 | TMap of type_ * type_
 | TChan of chan_dir * type_

 | TStruct    of struct_field list
 | TInterface of interface_field list

  and chan_dir = TSend | TRecv | TBidirectional
  and func_type =  { 
    fparams: parameter list; 
    fresults: parameter list;
  }
    and parameter = {
      pname: ident option;
      ptype: type_;
      (* only at last element position *)
      pdots: tok option;
    }

  and struct_field = struct_field_kind * tag option
    and struct_field_kind = 
    | Field of ident * type_ (* could factorize with entity *)
    | EmbeddedField of tok option (* * *) * qualified_ident
   and tag = string wrap
    
  and interface_field = 
    | Method of ident * func_type
    | EmbeddedInterface of qualified_ident

(* Id | Selector | Star *)
and expr_or_type = type_

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
and expr = 
 | BasicLit of literal
 (* the type of [...]{...} should be transformed in TArray (length {...}) *)
 | CompositeLit of type_ * init list

  (* can actually denotes a type sometimes, or a package *)
 | Id of ident 

 (* A Selector can be a 
  *  - a field access of a struct
  *  - a top decl access of a package
  *  - a method access when expr denotes actually a type 
  *  - a method value
  * We need more semantic information on expr to know what it is.
  *)
 | Selector of expr * tok * ident

 (* valid for TArray, TMap, Tptr, TName ("string") *)
 | Index of expr * index
  (* low, high, max *)
 | Slice of expr * (expr option * expr option * expr option) 

 | Call of call_expr
 | Deref of tok (* * *) * expr
 | Ref   of tok (* & *) * expr
 | Unary of         Ast_generic.arithmetic_operator (* +/-/~/! *) wrap * expr
 | Binary of expr * Ast_generic.arithmetic_operator wrap * expr
 | Receive of tok * expr

 | TypeAssert of expr * type_
 (* note that some Call are really Cast, e.g., uint(1), but we need
  * semantic information to know that
  *)
 | Cast of type_ * expr

 | Ellipsis of tok
 | FuncLit of func_type * stmt

  (* was just a string in ast.go *)
  and literal = 
  (* todo? Bool of bool wrap | Nil of tok? *)
  | Int of string wrap
  | Float of string wrap
  | Imag of string wrap
  | Rune of string wrap
  | String of string wrap

  and index = expr
  and arguments = argument list
  and argument = 
    | Arg of expr
    | ArgType of type_
    | ArgDots of tok (* should be the last argument *)

 (* could be merged with expr *)
 and init = 
  | InitExpr of expr (* can be Id, which have special meaning for Key *)
  | InitKeyValue of init * tok (* : *) * init
  | InitBraces of init list

and constant_expr = expr

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
and stmt = 
 | DeclStmts of decl list (* inside a Block *)

 | Empty
 | Block of stmt list

 | ExprStmt of expr
 (* good boy! not an expression but a statement! better! *) 
 | IncDec of expr * Ast_generic.incr_decr wrap * 
                    Ast_generic.prefix_postfix
 | Assign of expr list (* lhs *) * tok * expr list (* rhs *)

 | If     of stmt option (* init *) * expr * stmt * stmt option
 | Switch of stmt option (* init *) * expr * case_clause list
 | TypeSwitch of stmt option * stmt (* Assign *) * case_clause list
 | Select of comm_clause list

 (* no While or DoWhile, just For and Foreach (Range) *)
 | For of (stmt option * expr option * stmt option) * stmt
 | Range of (expr * expr option) (* key/value pattern *) * expr * stmt 

 | Return of tok * expr list option
 (* was put together in a Branch in ast.go, but better to split *)
 | Break of tok * ident option
 | Continue of tok * ident option
 | Goto of tok * ident 
 | Fallthrough of tok

 | Label of ident * stmt

 | Go    of tok * call_expr
 | Defer of tok * call_expr
 | Send of expr * tok (* <- *) * expr


 and case_clause = 
    expr_or_type list (* [] = default *) * stmt (* can be Empty*)
 and comm_clause =
    stmt (* Send or Receive *) * stmt (* can be empty *)
 and call_expr = expr * arguments

(*****************************************************************************)
(* Declarations *)
(*****************************************************************************)

and decl = 
 | DConst of ident * type_ option * constant_expr option 
 | DVar  of ident  * type_ option * (* = *) expr option (* value *)

 (* declare or reassign, and special semantic when Receive operation *)
 | DShortVars of ident list * tok (* := *) * expr list

 | DTypeAlias of ident * tok (* = *) * type_
 | DTypeDef of ident * type_

and top_decl =
 (* toplevel decl only *)
 | DFunc   of ident *                            func_type * stmt
 | DMethod of ident * parameter (* receiver *) * func_type * stmt
 | D of decl

(* ------------------------------------------------------------------------- *)
(* variable (local var, parameter) declaration *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* Function *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* Type *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* Struct *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
(* Interface *)
(* ------------------------------------------------------------------------- *)

(*****************************************************************************)
(* Import *)
(*****************************************************************************)
and import = {
 i_path: string wrap;
 i_kind: import_kind;
}
  and import_kind =
  | ImportOrig
  | ImportNamed of ident
  | ImportDot of tok

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)

type program = {
  package: ident;
  imports: import list;
  decls: top_decl list;
}

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type any = unit

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let stmt1 xs =
  match xs with
  | [] -> Empty
  | [st] -> st
  | xs -> Block xs
