(* Yoann Padioleau
 *
 * Copyright (C) 2021 R2C
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
(* An Abstract Syntax Tree for Scala.
 *
 * TODO:
 * - use the Tasty format?
 *   https://github.com/lampepfl/dotty/blob/master/tasty/src/dotty/tools/tasty/TastyFormat.scala
*)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
type tok = Parse_info.t
[@@deriving show]

(* a shortcut to annotate some information with token/position information *)
type 'a wrap = 'a * tok
[@@deriving show]

(* round(), square[], curly{}, angle<> brackets *)
type 'a bracket = tok * 'a * tok
[@@deriving show] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Names  *)
(* ------------------------------------------------------------------------- *)
(* can be a regular ident (e.g., "foo") or an operator (e.g., "**") or
 * even a backquoted ident (e.g., `foo is great`).
*)
type ident = string wrap
[@@deriving show] (* with tarzan *)

(* just used for prefixExpr *)
type op = string wrap
[@@deriving show] (* with tarzan *)

(* just for patterns, lowercase variable *)
type varid = string wrap
[@@deriving show] (* with tarzan *)

(* less: right now abusing ident to represent "_" *)
type ident_or_wildcard = ident
[@@deriving show] (* with tarzan *)
type varid_or_wildcard = ident
[@@deriving show] (* with tarzan *)



type dotted_ident = ident list
[@@deriving show] (* with tarzan *)

type path = dotted_ident
[@@deriving show] (* with tarzan *)
(* A stable identifier is a path which ends in an identifier
 * src: https://scala-lang.org/files/archive/spec/2.13/03-types.html
*)
type stable_id = dotted_ident
[@@deriving show] (* with tarzan *)

(* TODO:
   type path_element =
   | PId of ident
   | PThis of ident option * tok
   | PSuper of ident option * tok * id bracket option
   and path = path_element list
*)

type todo_category = string wrap
[@@deriving show] (* with tarzan *)

(*****************************************************************************)
(* Literal *)
(*****************************************************************************)

(* todo: interpolated strings? can be a literal pattern too *)
type literal =
  | Int    of int option wrap
  | Float  of float option wrap
  | Char   of string wrap
  | String of string wrap
  | Bool of bool wrap
  | Null of tok

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type type_ =
  | TyName of stable_id
  | TyProj of type_ * tok (* '#' *) * ident

  | TyApp of type_ * type_ list bracket
  | TyInfix of type_ * ident * type_
  | TyFunction of param_type list bracket * tok (* '=>' *) * type_
  | TyTuple of type_ list bracket

  (* todo: existentialClause (forSome), refinement *)
  | TyTodo of todo_category

and param_type =
  | ParamType of type_
  | ParamTypeArrow of tok (* => *) * type_
  | ParamTypeStar of type_ * tok (* * *)

(* todo: also _* or annotation list *)
and ascription = type_

(*****************************************************************************)
(* Patterns *)
(*****************************************************************************)
type pattern =
  | PatLiteral of literal
  | PatName of stable_id

  | PatVarid of varid_or_wildcard
  | PatTypedVarid of varid_or_wildcard * tok (* : *) * type_
  | PatAs of varid * tok (* @ *) * pattern

  (* less: the last pattern one can be '[varidd @] _ *' *)
  | PatCall of stable_id * pattern list bracket
  | PatInfix of pattern * ident * pattern
  | PatUnderscoreStar of tok (* '_' *) * tok (* '*' *)

  | PatDisj of pattern * pattern

  | PatTodo of todo_category

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)
type expr =
  | L of literal
  | Tuples of expr list bracket

  | Name of path
  | ExprUnderscore of tok (* '_' *)
  | InstanciatedExpr of expr * type_ list bracket (* ex: empty[List[Int]]? *)
  | TypedExpr of expr * tok (* : *) * ascription

  | DotAccess of expr * tok (* . *) * ident

  (* in Scala you can have multiple argument lists! This is
   * used in Scala for ArrAccess, implicits, etc.
  *)
  | Call of expr * arguments list

  | Infix of expr * ident * expr
  | Prefix of op (* just -/+/~/! *) * expr
  | Postfix of expr * ident

  | Assign of lhs * tok (* = *) * expr

  | Match of expr * tok (* 'match' *) * case_clauses bracket

  | Lambda of bindings * tok (* => *) * expr
  | New of tok (* TODO: ??? *)

  | S of stmt
  | D of definition
  | I of import

  | ExprTodo of todo_category

(* only Name, or DotAccess, or Call! (e.g., for ArrAccess) *)
and lhs = expr

and arguments = argument list bracket
and argument = expr

and case_clauses = case_clause list
and case_clause =
  tok (* 'case' *) * pattern * guard option * tok (* '=>' *) * block
and guard = tok (* 'if' *) * expr

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)
(* Note that in Scala everything is an expr, but I still like to split expr
 * with the different "subtypes" 'stmt' and 'definition'.
*)
and stmt =
  | Block of block bracket

  | If of tok * expr bracket * expr * (tok * expr) option
  | While of tok * expr bracket * expr
  | DoWhile of tok * expr * tok * expr bracket

  | For of enumerators bracket

  | Return of tok * expr option

  | Try of tok * expr * catch_clause option * finally_clause option
  | Throw of tok * expr

and enumerators = generator list
and generator =
  pattern * tok (* <- *) * expr * guard option

(* less: the last can be a ResultExpr *)
and block = block_stat list

and block_stat = expr

and catch_clause =
  tok * case_clauses bracket
and finally_clause=
  tok * block

(*****************************************************************************)
(* Attributes *)
(*****************************************************************************)
and attribute =
  | KeywordAttr of keyword_attribute wrap
  | NamedAttr of tok (* @ *) * ident * arguments bracket

and keyword_attribute =
  (* for traits/classes *)
  | Sealed
  (* for ??? *)
  | Private | Protected
  (* for ??? *)
  | Implicit

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
and definition = entity * definition_kind

and entity = {
  (* can be AST_generic.special_multivardef_pattern *)
  name: ident;
  (* type_: type; ? *)
  (* tparams: type_parameter list; *)
}

(* less: also work for declaration *)
and definition_kind =
  | FuncDef of function_definition
  | VarDef of variable_definition
  | TypeDef of type_definition
  (* class/traits/objects *)
  | Template of template_definition

(* ------------------------------------------------------------------------- *)
(* Val/Var *)
(* ------------------------------------------------------------------------- *)
and variable_definition = {
  (* move in entity? *)
  vtype: type_;
  vbody: expr;
}

(* ------------------------------------------------------------------------- *)
(* Typedef *)
(* ------------------------------------------------------------------------- *)
and type_definition = {
  (* move in entity? *)
  (* type_parameter list; *)
  tbody: type_;
}

(* ------------------------------------------------------------------------- *)
(* Functions/Methods *)
(* ------------------------------------------------------------------------- *)
and function_definition = {
  ftype: type_;
  fparams: bindings;
  fbody: expr;
}

(* fake bracket for single param in short lambdas *)
and bindings = binding list bracket
and binding =
  { p_name: ident_or_wildcard;
    p_type: type_ option;
    p_implicit: tok option; (* only when just one id in bindings *)
  }

(* ------------------------------------------------------------------------- *)
(* Traits/Classes/Objects *)
(* ------------------------------------------------------------------------- *)

(* =~ class def, hence the c prefix below *)
and template_definition = {
  ckind: template_kind wrap;
  cbody: block bracket;
}
and template_kind =
  | Class
  | Trait
  | Object

(*****************************************************************************)
(* Directives *)
(*****************************************************************************)
and import = unit

(*****************************************************************************)
(* Toplevel elements *)
(*****************************************************************************)

type program = unit

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type any =
  | Program of program
  | Tk of tok

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
