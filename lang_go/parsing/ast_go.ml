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
(* for? *)
type ident = string wrap
 (* with tarzan *)

(* for ?  (called names in ast.go) *)
type qualified_ident = ident list (* 1 or 2 elements *)
 (* with tarzan *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
type type_ =
 | TName of qualified_ident
 | TPtr of type_

 | TArray of array_kind * type_
 | TFunc of func_type
 | TMap of type_ * type_
 | TChan of chan_dir * type_

 | TStruct    of struct_field list
 | TInterface of interface_field list

  and chan_dir = TSend | TRecv | TBidirectional
  and array_kind = TSlice of expr option | TEllipsis of tok
  and func_type =  { 
    fparams: parameter list; 
    fresults: parameter list;
  }
    and parameter = {
      pname: ident option;
      ptype: type_ option; (* None only for pdots *)
      pdots: (tok * type_ option) option;
    }

  and struct_field = unit
  and interface_field = unit

(* Id | Selector | Star *)
and expr_or_type = type_

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
and expr = 
 | BasicLit of literal
 | CompositeLit of type_ * init list

 | Id of ident

 | Selector of expr * selector
 (* valid for TArray, TMap, Tptr, TName ("string") *)
 | Index of expr * index
  (* low, high, max *)
 | Slice of expr * (expr option * expr option * expr option) 

 | Call of expr * arguments
 | Star of tok * expr
 | Unary of         Ast_generic.arithmetic_operator (* +/-/~/! *) wrap * expr
 | Binary of expr * Ast_generic.arithmetic_operator wrap * expr
 | Receive of tok * expr

 | TypeAssert of expr * type_

 | Ellipsis of tok
 | FuncLit of func_type * stmt

  (* was just a string in ast.go *)
  and literal = 
  | Int of string wrap
  | Float of string wrap
  | Imag of string wrap
  | Rune of string wrap
  | String of string wrap

  and selector = ident
  and index = expr
  and arguments = argument list
  and argument = 
    | Arg of expr
    | ArgType of type_
    | ArgDots of tok (* should be the last argument *)

 (* could be merged with expr *)
 and init = 
  | InitExpr of expr
  | InitKeyValue of init * tok (* : *) * init
  | InitBraces of init list

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
and stmt = 
 | DeclStmt of decl (* inside a Block *)

 | Empty
 | Block of stmt list

 | ExprStmt of expr
 (* good boy! not an expression but a statement! better! *) 
 | IncDec of expr * Ast_generic.incr_decr wrap * 
                    Ast_generic.prefix_postfix wrap
 | Assign of expr list (* lhs *) * tok * expr list (* rhs *)

 | If of stmt option (* init *) * expr * stmt * stmt option
 | Switch of stmt option (* init *) * expr * case_clause list
 | TypeSwitch of stmt option * stmt (* Assign *) * case_clause list
 | Select of comm_clause list

 (* no While or DoWhile, just For and Foreach (Range) *)
 | For of (stmt option * expr option * stmt option) * stmt
 | Range of (expr * expr option) (* key/value pattern *) * expr * stmt 

 | Return of tok * expr option
 | Branch of branch_kind wrap * ident option

 | Label of ident * stmt

 | Go of tok * call_expr
 | Send of expr * tok (* <- *) * expr

 | Defer of tok * call_expr

 and branch_kind = Break | Continue | Goto | Fallthrough
 and case_clause = 
    expr_or_type list (* [] = default *) * stmt (* can be Empty*)
 and comm_clause =
    stmt (* Send or Receive *)* stmt (* can be empty *)
 and call_expr = expr * arguments

(*****************************************************************************)
(* Declarations *)
(*****************************************************************************)

and entity = {
    name: ident;
    type_: type_ option;
    (* could put more stuff here later *)
 }

and decl = entity * declaration_kind

and declaration_kind = 
 | DVar of expr option (* value *)
 | DConst of expr option 
 | DType of expr_or_type
 | DFunc of field option (* receiver *) * func_type * stmt

and field = {
    fld_name: qualified_ident option;
    fld_type: type_;
    (* only in args *)
    fld_dots: (tok * type_ option) option;
 }

and fields = field list 

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
  decls: decl list;
}

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type any = unit

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let mk_entity id topt = 
  { name = id; type_ = topt }

let _mk_field qid_opt t = 
  { fld_name = qid_opt; fld_type = t; fld_dots = None }

let mk_param _nopt _topt =
  raise Common.Todo