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
(* A Concrete/Abstract Syntax Tree for Scala 2.
 *
 * I tried to keep the names used in the original compiler for
 * the AST constructs (e.g., Template for class/traits/objects, PatBind
 * for what I usually call PatAs, Apply for Call, bindings for parameters,
 * PatApply for Constructor, etc.),
 * or corresponding grammar rules (e.g., block_stat, block_expr, import_expr).
 * In case I didn't, I used the ast_orig: tag to indicate what was the
 * original name.
 *
 * See the scala3: tag for possible extensions to handle Scala 3.
 *
 * alt:
 * - mimic the AST types/classes in the Scala compiler, but they look
 *   very weakly typed (not as bad as just Node/Leaves, but not
 *   super precise either)
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
[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Names  *)
(* ------------------------------------------------------------------------- *)
(* can be a regular ident (e.g., "foo") or an operator (e.g., "**") or
 * even a backquoted ident (e.g., `foo is great`).
*)
type ident = string wrap
[@@deriving show]

(* just used for prefixExpr *)
type op = string wrap
[@@deriving show]

(* just for patterns, lowercase ident *)
type varid = string wrap
[@@deriving show]

let wildcard = "_"
let this = "this"
let super = "super"

(* less: right now abusing ident to represent "_" *)
type ident_or_wildcard = ident
[@@deriving show]
type varid_or_wildcard = ident
[@@deriving show]
(* less: right now abusing ident to represent "this" *)
type ident_or_this = ident
[@@deriving show]


type dotted_ident = ident list
[@@deriving show]

(* just for packages for now *)
type qualified_ident = dotted_ident
[@@deriving show]

(* scala3: called simple_ref *)
type path = dotted_ident
[@@deriving show]

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
[@@deriving show]


(* TODO: to remove at some point when the AST is finished *)
type todo_category = string wrap
[@@deriving show]

(*****************************************************************************)
(* Directives *)
(*****************************************************************************)

type import_selector = ident_or_wildcard * alias option
and alias = tok (* => *) * ident_or_wildcard
[@@deriving show]

type import_expr = stable_id * import_spec
and import_spec =
  | ImportId of ident
  | ImportWildcard of tok (* '_' *)
  | ImportSelectors of import_selector list bracket
[@@deriving show {with_path = false }]

type import = tok (* 'import' *) * import_expr list
[@@deriving show]

type package = tok (* 'package' *) * qualified_ident
[@@deriving show]

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
[@@deriving show {with_path = false }]

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type type_ =
  (* scala3: simple_literal, ast_orig: SingletonType *)
  | TyLiteral of literal (* crazy? genius? *)
  | TyName of stable_id

  (* ast_orig: SelectFromType *)
  | TyProj of type_ * tok (* '#' *) * ident
  (* ast_orig: AppliedType *)
  | TyApplied of type_ * type_ list bracket

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
[@@deriving show {with_path = false }]

(* todo: also _* or annotation list *)
type ascription = type_
[@@deriving show]

(*****************************************************************************)
(* Patterns *)
(*****************************************************************************)
type pattern =
  (* interpolated strings serve as regexp-like patterns (nice) *)
  | PatLiteral of literal
  | PatName of stable_id
  | PatTuple of pattern list bracket

  | PatVarid of varid_or_wildcard
  (* ast_orig: just Typed *)
  | PatTypedVarid of varid_or_wildcard * tok (* : *) * type_
  | PatBind of varid * tok (* @ *) * pattern

  (* less: the last pattern one can be '[varid @] _ *'
   * ast_orig: AppliedType for the type_ list bracket
  *)
  | PatApply of stable_id *
                type_ list bracket option *
                pattern list bracket option
  | PatInfix of pattern * ident * pattern
  (* less: only last element of a pattern list? *)
  | PatUnderscoreStar of tok (* '_' *) * tok (* '*' *)

  | PatDisj of pattern * tok (* | *) * pattern

  | PatTodo of todo_category
[@@deriving show {with_path = false }]

(*****************************************************************************)
(* Start of big recursive type *)
(*****************************************************************************)
(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)
type expr =
  | L of literal
  | Tuple of expr list bracket

  | Name of path
  | ExprUnderscore of tok (* '_' *)

  | InstanciatedExpr of expr * type_ list bracket (* ex: empty[List[Int]]? *)
  | TypedExpr of expr * tok (* : *) * ascription

  | DotAccess of expr * tok (* . *) * ident

  (* in Scala you can have multiple argument lists! This is
   * used in Scala for ArrAccess, implicits, block as last argument, etc.
  *)
  | Apply of expr * arguments list

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
  | New of tok * template_definition
  | BlockExpr of block_expr

  | S of stmt

  | ExprTodo of todo_category

(* only Name, or DotAccess, or Apply! (e.g., for ArrAccess) *)
and lhs = expr

and arguments =
  | Args of argument list bracket
  (* Ruby-style last argument used as a block (nice when defining your
   * own control structure) *)
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
 * also put 'definition' as a "subtype" but in Scala we can restrict
 * them to appear only in block_stat (see block_stat below).
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
(* less: use a record? *)
and generator =
  pattern * tok (* <- or = *) * expr * guard list
and for_body =
  | Yield of tok * expr
  | NoYield of expr

and catch_clause =
  tok (* 'catch' *) * (* TODO: case_clauses bracket *) expr
and finally_clause=
  tok(* 'finally' *) * expr

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
  | Package of package
  | Packaging of package * top_stat list bracket

(* those have special restrictions but simpler to make them alias
 * to block_stat. Anyway in AST_generic they will be all converted
 * to stmts/items.
*)
and template_stat = block_stat
and top_stat = block_stat

(*****************************************************************************)
(* Attributes (modifiers and annotations) *)
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
  (* less: rewrite as Packaging and object def like in original code? *)
  | PackageObject
  (* just for variables/fields/class params *)
  | Val (* immutable *)
  | Var (* mutable *)

and annotation =
  tok (* @ *) * type_ (* usually just a TyName*) * arguments list

and attribute =
  | A of annotation
  | M of modifier

(*****************************************************************************)
(* Type parameter (generics) *)
(*****************************************************************************)
(* TODO *)
and type_parameter = unit

and type_bounds = {
  supertype: (tok (* >: *) * type_) option;
  subtype:   (tok (* <: *) * type_) option;
}

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
(* definition or declaration (def or dcl) *)
and definition =
  | DefEnt of entity * definition_kind
  | VarDefs of variable_definitions

(* ------------------------------------------------------------------------- *)
(* Val/Var entities *)
(* ------------------------------------------------------------------------- *)
(* Used for local variables but also for fields *)
and variable_definitions = {
  (* a bit like entity, but for a list of stuff because of the pattern *)
  vpatterns: pattern list;
  vattrs: attribute list;

  (* old: vkind: variable_kind wrap;, now in vattrs *)
  vtype: type_ option;
  vbody: expr option; (* None for declarations *)
}

(* ------------------------------------------------------------------------- *)
(* Other entities *)
(* ------------------------------------------------------------------------- *)

and entity = {
  (* can be "this" for constructor *)
  name: ident;
  attrs: attribute list;
  tparams: type_parameter list;
}

(* less: also work for declaration, in which case the [fc]body is empty *)
and definition_kind =
  | FuncDef of function_definition
  | TypeDef of type_definition
  (* class/traits/objects *)
  | Template of template_definition

(* ------------------------------------------------------------------------- *)
(* Functions/Methods *)
(* ------------------------------------------------------------------------- *)
and function_definition = {
  fkind: function_kind wrap;
  (* a list of list of parameters! but usually 0/1/2 *)
  fparams: bindings list;
  (* scala3? remove None and force : Unit ? *)
  frettype: type_ option;
  fbody: fbody option; (* None for declarations *)
}
and function_kind =
  | LambdaArrow (* '=>' *)
  | Def (* 'def' *)
(* less: Constructor, when name = "this"? *)

and fbody =
  | FBlock of block_expr
  | FExpr of tok (* = (or => for lambdas) *) * expr

(* fake brackets for single param in short lambdas *)
and bindings = binding list bracket
and binding = {
  p_name: ident_or_wildcard;
  (* especially var/val, and implicit *)
  p_attrs: attribute list;
  (* None only in Lambdas; Def must define types for each parameters *)
  p_type: param_type option;
  p_default: expr option;
}

(* ------------------------------------------------------------------------- *)
(* Traits/Classes/Objects *)
(* ------------------------------------------------------------------------- *)

(* =~ class def, hence the c prefix below *)
and template_definition = {
  ckind: template_kind wrap;
  (* also a list of list of parameters *)
  cparams: bindings list;
  cparents: template_parents;
  cbody: block bracket option;
}
(* scala3: intersection types so more symetric *)
and template_parents = {
  cextends: type_ option (* TODO: * arguments list ??? *);
  cwith: type_ list
}

(* Case classes/objects are handled via attributes in the entity *)
and template_kind =
  | Class
  | Trait
  | Object
  | Singleton (* via new *)

(* ------------------------------------------------------------------------- *)
(* Typedef *)
(* ------------------------------------------------------------------------- *)
and type_definition = {
  ttok: tok; (* 'type' *)
  tbody: type_definition_kind;
}
and type_definition_kind =
  | TDef of tok (* = *) * type_
  | TDcl of type_bounds

[@@deriving show {with_path = false }]

(*****************************************************************************)
(* Toplevel elements *)
(*****************************************************************************)

type program = top_stat list
[@@deriving show]

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type any =
  | Program of program
  | Tk of tok
[@@deriving show {with_path = false }]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let empty_cparents = { cextends = None; cwith = [] }

let attrs_of_mods xs = List.map (fun x -> M x) xs
let attrs_of_annots xs = List.map (fun x -> A x) xs
let mods_with_annots mods annots = attrs_of_annots annots @ attrs_of_mods mods

(* Intermediate type just used during parsing.
 * less: move in the parser code instead.
*)
type literal_or_interpolated = (literal, expr) Common.either
