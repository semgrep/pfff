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
(* An Abstract Syntax Tree for Scala 2.
 *
 * See also the scala3: tag for possible extensions to handle Scala 3.
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
(* less: right now abusing ident to represent "this" *)
type ident_or_this = ident
[@@deriving show] (* with tarzan *)



type dotted_ident = ident list
[@@deriving show] (* with tarzan *)

(* just for packages for now *)
type qualified_ident = dotted_ident
[@@deriving show] (* with tarzan *)

(* scala3: called simple_ref *)
type path = dotted_ident
[@@deriving show] (* with tarzan *)

(* TODO:
   scala3: called simple_ref
   type simple_ref =
   | PId of ident
   | PThis of ident option * tok
   | PSuper of ident option * tok * id bracket option * ident
   and path = path_element list
   type path = simple_ref * dotted_ident ?
*)

(* A stable identifier is a path which ends in an identifier
 * src: https://scala-lang.org/files/archive/spec/2.13/03-types.html
*)
type stable_id = dotted_ident
[@@deriving show] (* with tarzan *)

type todo_category = string wrap
[@@deriving show] (* with tarzan *)

(*****************************************************************************)
(* Directives *)
(*****************************************************************************)

type import_selector = ident_or_wildcard * alias option
and alias = tok (* => *) * ident_or_wildcard

type import_expr = stable_id * import_spec
and import_spec =
  | ImportId of ident
  | ImportWildcard of tok (* '_' *)
  | ImportSelectors of import_selector list bracket

type import = tok (* 'import' *) * import_expr list

type package = tok (* 'package' *) * qualified_ident

(*****************************************************************************)
(* Literals *)
(*****************************************************************************)

(* todo: interpolated strings? can be a literal pattern too?
 * scala3: called simple_literal
*)
type literal =
  | Int    of int option wrap
  | Float  of float option wrap
  | Char   of string wrap
  | String of string wrap
  | Bool of bool wrap
  (* scala3: not in simple_literal *)
  | Null of tok

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type type_ =
  (* scala3: simple_literal *)
  | TyLiteral of literal (* crazy? genius? *)
  | TyName of stable_id
  | TyProj of type_ * tok (* '#' *) * ident

  | TyApp of type_ * type_ list bracket
  | TyInfix of type_ * ident * type_
  | TyFunction1 of type_ * tok (* '=>' *) * type_
  | TyFunction2 of param_type list bracket * tok (* '=>' *) * type_
  | TyTuple of type_ list bracket

  (* todo: existentialClause (forSome), refinement *)
  | TyTodo of todo_category

and param_type =
  | PT of type_
  | PTByNameApplication of tok (* => *) * type_
  | PTRepeatedApplication of type_ * tok (* * *)

(* todo: also _* or annotation list *)
type ascription = type_

(*****************************************************************************)
(* Patterns *)
(*****************************************************************************)
type pattern =
  (* interpolated strings serve as regexp-like patterns (nice) *)
  | PatLiteral of literal
  | PatName of stable_id

  | PatVarid of varid_or_wildcard
  | PatTypedVarid of varid_or_wildcard * tok (* : *) * type_
  | PatBind of varid * tok (* @ *) * pattern

  (* less: the last pattern one can be '[varid @] _ *' *)
  | PatCall of stable_id * pattern list bracket
  | PatInfix of pattern * ident * pattern
  | PatUnderscoreStar of tok (* '_' *) * tok (* '*' *)

  | PatDisj of pattern * tok (* | *) * pattern

  | PatTodo of todo_category

(*****************************************************************************)
(* start of big recursive type? *)
(*****************************************************************************)
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
   * used in Scala for ArrAccess, implicits, block as last argument, etc.
  *)
  | Call of expr * arguments list

  (* in Scala any identifier can be used in infix position
   * (nice but also easy to abuse).
   * scala3: restricted to functions declared as 'infix'
  *)
  | Infix of expr * ident * expr
  | Prefix of op (* just -/+/~/! *) * expr
  | Postfix of expr * ident

  | Assign of lhs * tok (* = *) * expr

  | Match of expr * tok (* 'match' *) * case_clauses bracket

  | Lambda of function_definition
  | New of tok (* TODO * template_definition *)

  | S of stmt

  | ExprTodo of todo_category

(* only Name, or DotAccess, or Call! (e.g., for ArrAccess) *)
and lhs = expr

and arguments =
  | Args of argument list bracket
  (* Ruby-style last argument used as a block (nice when defining your
   * own control structure.
  *)
  | ArgBlock of block_expr
  (* less: no keyword argument in Scala? *)
and argument = expr

and case_clauses = case_clause list
(* less: use a record? *)
and case_clause =
  tok (* 'case' *) * pattern * guard option * tok (* '=>' *) * block
and guard = tok (* 'if' *) * expr

and block_expr = block_expr_kind bracket
and block_expr_kind =
  | BEBlock of block
  | BECases of case_clauses

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)
(* Note that in Scala everything is an expr, but I still like to split expr
 * with the different "subtype" 'stmt'. In some languages, e.g., Ruby, I
 * also put 'definition' as a "subtype" but in Scala we can restricte
 * them to block_stat below.
*)
and stmt =
  | Block of block bracket

  | If of tok * expr bracket * expr * (tok * expr) option
  | While of tok * expr bracket * expr
  | DoWhile of tok * expr * tok * expr bracket

  | For of tok * enumerators bracket * for_body

  | Return of tok * expr option

  | Try of tok * expr * catch_clause option * finally_clause option
  | Throw of tok * expr

and enumerators = generator list
and generator =
  pattern * tok (* <- or = *) * expr * guard list
and for_body =
  | Yield of tok * expr
  | NoYield of expr

and catch_clause =
  tok * (* TODO: case_clauses bracket *) expr
and finally_clause=
  tok * expr

(*****************************************************************************)
(* XxxStats *)
(*****************************************************************************)
(* less: the last can be a ResultExpr *)
and block = block_stat list

(* pad: not sure what Stat means in original grammar. Statement? *)
and block_stat =
  | D of definition
  | I of import
  | E of expr
  (* just at the beginning of top_stat *)
  | P of package

  | BlockTodo of todo_category

(* those have special restrictions but simpler to make them alias
 * to block_stat. Anyway in AST_generic they will be all converted
 * to stmts/items.
*)
and template_stat = block_stat
and top_stat = block_stat

(*****************************************************************************)
(* Attributes *)
(*****************************************************************************)
and modifier = modifier_kind wrap
and modifier_kind =
  (* local modifier *)
  | Abstract
  | Final
  (* scala specific *)
  | Sealed
  | Implicit
  | Lazy
  (* access modifier *)
  | Private of ident_or_this bracket option
  | Protected of ident_or_this bracket option
  (* misc (and nice!) *)
  | Override
  (* pad: not in original spec *)
  | CaseClassOrObject
  | PackageObject

and annotation = tok (* @ *) * type_ * arguments list

and attribute =
  | A of annotation
  | M of modifier

(*****************************************************************************)
(* Type parameter (generics) *)
(*****************************************************************************)
and type_parameter = unit

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
(* definition or declaration (def or dcl) *)
and definition =
  | DefEnt of entity * definition_kind
  | DefTodo of todo_category

and entity = {
  (* can be AST_generic.special_multivardef_pattern? *)
  name: ident;
  attrs: attribute list;
  tparams: type_parameter list;
}

(* less: also work for declaration, in which case the xbody is empty *)
and definition_kind =
  | FuncDef of function_definition
  | VarDef of variable_definition
  | TypeDef of type_definition
  (* class/traits/objects *)
  | Template of template_definition

(* TODO: multiPatDef? *)

(* ------------------------------------------------------------------------- *)
(* Val/Var *)
(* ------------------------------------------------------------------------- *)
(* Used for local variables but also for fields *)
and variable_definition = {
  vkind: variable_kind wrap;
  vtype: type_; (* option? *)
  vbody: expr option; (* None for declarations? *)
}
and variable_kind =
  | Val (* immutable *)
  | Var (* mutable *)

(* ------------------------------------------------------------------------- *)
(* Functions/Methods *)
(* ------------------------------------------------------------------------- *)
and function_definition = {
  fkind: function_kind wrap;
  fparams: bindings;
  (* scala3? remove None and force : Unit ? *)
  frettype: type_ option;
  fbody: expr option; (* None for declarations *)
}
and function_kind =
  | LambdaArrow (* '=>' *)
  | Def (* 'def' *)

(* fake brackets for single param in short lambdas *)
and bindings = binding list bracket
and binding = {
  p_name: ident_or_wildcard;
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
(* case classes and objects are handled via attributes in the entity *)
and template_kind =
  | Class
  | Trait
  | Object

(* ------------------------------------------------------------------------- *)
(* Typedef *)
(* ------------------------------------------------------------------------- *)
and type_definition = {
  tbody: type_;
}

(*****************************************************************************)
(* Toplevel elements *)
(*****************************************************************************)

type program = top_stat list

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type any =
  | Program of program
  | Tk of tok

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

(* Intermediate type just used during parsing.
 * less: move in the parser code instead.
*)
type literal_or_interpolated = (literal, expr) Common.either
