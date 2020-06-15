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
(* PPX *)
(*****************************************************************************)
let pp_tok fmt _ = Format.fprintf fmt "()"

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
 [@@deriving show] (* with tarzan *)

(* round(), square[], curly{}, angle<> brackets *)
type 'a bracket = tok * 'a * tok
 [@@deriving show] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Ident, qualifier *)
(* ------------------------------------------------------------------------- *)
(* For functions/methods/parameters/fields/labels *)
type ident = string wrap
 [@@deriving show] (* with tarzan *)

(* For type names  (called names in ast.go). It could also be used for
 * imported entities from other module, but they are currently parsed as
 * a Selector (Id, Id) instead of a qualified_ident because of ambiguities
 * that require a semantic analysis to disambiguate.
 *)
type qualified_ident = ident list (* 1 or 2 elements *)
 [@@deriving show] (* with tarzan *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
type type_ =
 | TName of qualified_ident (* includes the basic types: bool/int/... *)
 | TPtr of tok * type_

 | TArray of expr * type_
 | TSlice of type_
  (* only in CompositeLit (could be rewritten as TArray with static length) *)
 | TArrayEllipsis of tok (* ... *) * type_ 

 | TFunc of func_type
 | TMap of tok * type_ * type_
 | TChan of tok * chan_dir * type_

 | TStruct    of tok * struct_field list bracket
 | TInterface of tok * interface_field list bracket

  and chan_dir = TSend | TRecv | TBidirectional
  and func_type =  { 
    fparams: parameter_binding list; 
    fresults: parameter_binding list;
  }
    and parameter_binding = 
     | ParamClassic of parameter
     (* sgrep-ext: *)
     | ParamEllipsis of tok
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

and expr_or_type = (expr, type_) Common.either

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
and expr = 
 | BasicLit of literal
 (* less: the type of TarrayEllipsis ( [...]{...}) in a CompositeLit
  *  could be transformed in TArray (length {...}) *)
 | CompositeLit of type_ * init list bracket

  (* This Id can actually denotes sometimes a type (e.g., in Arg), or 
   * a package (e.g., in Selector). 
   * To disambiguate requires semantic information.
   * Selector (Name,'.', ident) can be many things.
   *)
 | Id of ident * AST_generic.resolved_name option ref

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
 (* note that some Call are really Cast, e.g., uint(1), but we need
  * semantic information to know that. Actually, some Cast
  * such as (Foo)(nil) are unfortunately parsed as Calls, because again
  * you need typing information to know that.
  *)
 | Cast of type_ * expr

 (* special cases of Unary *)
 | Deref of tok (* * *) * expr
 (* less: some &T{...} should be transformed in call to new? *)
 | Ref   of tok (* & *) * expr
 | Receive of tok * expr (* denote a channel *)

 | Unary of         AST_generic.arithmetic_operator (* +/-/~/! *) wrap * expr
 | Binary of expr * AST_generic.arithmetic_operator wrap * expr

 (* x.(<type>), panic if false unless used as x, ok = x.(<type>) *)
 | TypeAssert of expr * type_
 (* x.(type)
  * less: can appear only in a TypeSwitch, so could be moved there *)
 | TypeSwitchExpr of expr * tok (* 'type' *)

 (* sgrep-ext: *)
 | Ellipsis of tok
 | DeepEllipsis of expr bracket
 | TypedMetavar of ident * tok * type_

 | FuncLit of function_

 (* only used as an intermediate during parsing, should be converted *)
 | ParenType of type_

 (* TODO: move in stmt, but need better comm_clause *)
 (* Send as opposed to Receive is a statement, not an expr *)
 | Send of expr (* denote a channel *) * tok (* <- *) * expr

  (* old: was just a string in ast.go *)
  and literal = 
  (* less: Bool of bool wrap | Nil of tok? *)
  | Int of string wrap
  | Float of string wrap
  | Imag of string wrap
  | Rune of string wrap (* unicode char *)
  | String of string wrap (* unicode string *)

  and index = expr
  and arguments = argument list
  and argument = 
    (* less: could also use Arg of expr_or_type *)
    | Arg of expr
    (* for new, make, ?? *)
    | ArgType of type_
    | ArgDots of expr * tok (* should be the last argument *)

 (* could be merged with expr *)
 and init = 
  | InitExpr of expr (* can be Id, which have special meaning for Key *)
  | InitKeyValue of init * tok (* : *) * init
  | InitBraces of init list bracket

and constant_expr = expr

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
and stmt = 
 | DeclStmts of decl list (* inside a Block *)

 | Block of stmt list
 (* less: could be rewritten as Block [] *)
 | Empty

 | SimpleStmt of simple

 | If     of tok * simple option (* init *) * expr * stmt * stmt option
 (* todo: cond should be an expr, except for TypeSwitch where it can also
  * be x := expr
  *)
 | Switch of tok * simple option (* init *) * simple option * case_clause list
 (* todo: expr should always be a TypeSwitchExpr *)
 (* | TypeSwitch of stmt option * expr (* Assign *) * case_clause list *)
 | Select of tok * comm_clause list

 (* note: no While or DoWhile, just For and Foreach (Range) *)
 | For of tok * (simple option * expr option * simple option) * stmt
 (* todo: should impose (expr * tok * expr option) for key/value *)
 | Range of tok * 
      (expr list * tok (* = or := *)) option (* key/value pattern *) * 
      tok (* 'range' *) * expr * stmt 

 | Return of tok * expr list option
 (* was put together in a Branch in ast.go, but better to split *)
 | Break    of tok * ident option
 | Continue of tok * ident option
 | Goto     of tok * ident 
 | Fallthrough of tok

 | Label of ident * stmt

 | Go    of tok * call_expr
 | Defer of tok * call_expr

 (* todo: split in case_clause_expr and case_clause_type *)
 and case_clause = case_kind * stmt (* can be Empty*)
   and case_kind =
    | CaseExprs of tok * expr_or_type list
    | CaseAssign of tok * expr_or_type list * tok (* = or := *) * expr
    | CaseDefault of tok
 (* TODO: stmt (* Send or Receive *) * stmt (* can be empty *) *)
 and comm_clause = case_clause
    
 and call_expr = expr * arguments

 and simple =
 | ExprStmt of expr
 (* good boy! not an expression but a statement! better! *) 
 (* note: lhs and rhs do not always have the same length as in
  *  a,b = foo()
  *)
 | Assign of expr list (* lhs, pattern *) * tok * expr list (* rhs *)
 | AssignOp of expr * AST_generic.arithmetic_operator wrap * expr
 | IncDec of expr * AST_generic.incr_decr wrap * AST_generic.prefix_postfix
 (* declare or reassign, and special semantic when Receive operation *)
 | DShortVars of expr list * tok (* := *) * expr list

(*****************************************************************************)
(* Declarations *)
(*****************************************************************************)

and decl = 
 (* consts can have neither a type nor an expr but the expr is usually
  * a copy of the expr of the previous const in a list of consts (e.g., iota),
  * and the grammar imposes that the first const at least has an expr.
  * less: could do this transformation during parsing.
  *)
 | DConst of ident * type_ option * constant_expr option 
 (* vars have at least a type or an expr ((None,None) is impossible) *)
 | DVar   of ident * type_ option * (* = *) expr option (* value *)

 (* type can be a TStruct to define and name a structure *)
 | DTypeAlias of ident * tok (* = *) * type_
 (* this introduces a distinct type, with different method set *)
 | DTypeDef of ident * type_
 (* with tarzan *)

and function_ = func_type * stmt

 [@@deriving show { with_path = false }]

(* only at the toplevel *)
type top_decl =
 | DFunc   of ident *                            function_
 | DMethod of ident * parameter (* receiver *) * function_
 | D of decl

 [@@deriving show { with_path = false }] (* with tarzan *)

(*****************************************************************************)
(* Import *)
(*****************************************************************************)
type import = {
 i_tok: tok;
 i_path: string wrap;
 i_kind: import_kind;
}
  and import_kind =
  (* basename of i_path is usually the package name *)
  | ImportOrig
  | ImportNamed of ident
  (* inline in current file scope all the entities of the imported module *)
  | ImportDot of tok
 [@@deriving show { with_path = false }] (* with tarzan *)

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)

type program = {
  package: tok * ident;
  imports: import list;
  decls: top_decl list;
}
 [@@deriving show { with_path = false }] (* with tarzan *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

(* this is just for sgrep *)
type item = 
  | ITop of top_decl
  | IImport of import
  | IStmt of stmt

 [@@deriving show { with_path = false }]
 
type any = 
 | E of expr
 | S of stmt
 | T of type_
 | Decl of decl
 | I of import
 | P of program

 | Ident of ident
 | Ss of stmt list
 | Item of item
 | Items of item list

 [@@deriving show { with_path = false }] (* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let stmt1 xs =
  match xs with
  | [] -> Empty
  | [st] -> st
  | xs -> Block xs

let item1 xs =
  match xs with
  | [] -> raise Common.Impossible
  | [x] -> Item x
  | xs -> Items xs

let str_of_id (s,_) = s
