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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A generic AST, to be used by a generic visitor to factorize
 * similar analysis in different programming languages
 * (e.g., checked_return). 
 *
 * rational: In the end, programming languages have a lot in common.
 * Even though most interesting analysis are probably better done on a
 * per-language basis, many analysis are trivial and requires just an
 * AST and a visitor. One could duplicate those analysis for each language
 * or design ast_generic to be generic enough and precise enough to 
 * factorize those analysis (e.g., checked_return).
 * 
 * TODO:
 *  - initial goal: factorize Python, Javascript
 *  - later: add Go, Java.
 *  - later: add Ruby, Rust, Scala.
 *  - later: add C++, C.
 *  - dogfooding: add OCaml!
 *
 * related work:
 *  - ast_fuzzy.ml
 *  - github semantic
 *    https://github.com/github/semantic
 *  - Coverity common program representation?
 *  - Semmle internal common represetation?
 *  - Infer SIL (for C++, Java, Objective-C)
 *
 * design choices to have a generic data structure:
 *  - add some 'a, 'b, 'c around expr/stmt/...
 *  - functorize and add some type hole (type tstmt; type texpr; ...)
 *  - data-type a la carte like in github-semantic? Seems too high-level,
 *    astronaut-style architecture (too abstract, too advanced features).
 *)

(*****************************************************************************)
(* Types *)
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

(* todo? modules? namespaces? *)
type qualified_name = name
 (* with tarzan *)

(* todo: see also scope_code.ml *)
type resolved_name =
  | Local
  | Param
  | Global of qualified_name
  | NotResolved
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
(* big mutually recursive types because of the use of 'any' in OtherXxx *)

type expr = 
  (* basic values (literals) *)
  | Bool of bool wrap
  | Int of string wrap
  | Float of string wrap
  | String of string wrap
  (* less: Regexp of string wrap, Imag of string wrap *)

  (* composite values *)
  | Container of container_operator * expr list

  | Nop

  | Id of name * resolved_name ref
  (* less: IdSpecial *)

  (* operators and function application *)
  | BinaryOp of expr * binary_operator wrap * expr
  | UnaryOp of unary_operator * expr
  | Call of expr * argument list * other_arguments

  | ObjAccess of expr * name
  | ArrayAccess of expr * expr

  | Lambda of parameters * stmt
  | Conditional of expr * expr * expr

  | OtherExpr of other_expr_operator * any list

  and binary_operator = 
    | Add | Sub | Mult | Div | Mod 
    (* less:  Pow  | FloorDiv *)
    | LShift | RShift 
    | BitOr | BitXor | BitAnd 
    | And | Or
    | Eq | NotEq 
    | Lt | LtE | Gt | GtE 
  
  and unary_operator =
    | UPlus | USub | UNot

  and container_operator = 
    | Tuple
    | Array
    | List
    | Dict
    | Set

  and argument =
    | Arg of expr
    | ArgKwd of name * expr

  and other_arguments = XXXOA

  and other_expr_operator = XXX1


(* ------------------------------------------------------------------------- *)
(* Type *)
(* ------------------------------------------------------------------------- *)
and type_ =
  | TBuiltin of string wrap
  | OtherType of other_type_operator * any list

  and other_type_operator = XXOTO

(* ------------------------------------------------------------------------- *)
(* Annotation *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
and stmt =
  | ExprStmt of expr
  | Block of stmt list

  | If of expr * stmt * stmt

  | Try of stmt * catch list * finally option

  | OtherStmt of other_stmt_operator * any list

  and catch = stmt
  and finally = stmt

  and other_stmt_operator = XXX2
  
(* ------------------------------------------------------------------------- *)
(* Function (or method) definition *)
(* ------------------------------------------------------------------------- *)
and function_definition = {
 fname: name;
 fparams: parameters;
 ftype: type_ option; (* return type *)
 fbody: stmt;
}
  and parameters = parameter list * other_parameters

    and parameter = { 
     pname: name;
     pdefault: expr option;
     ptype: type_ option;
    }
  and other_parameters = XXXOP

(* ------------------------------------------------------------------------- *)
(* Variable definition *)
(* ------------------------------------------------------------------------- *)
and variable_definition = {
  vname: name;
  vinit: expr option;
  vtype: type_ option;
}

(* ------------------------------------------------------------------------- *)
(* Class definition *)
(* ------------------------------------------------------------------------- *)
and class_definition = {
  cname: name;
  cparents: expr list;
  cbody: stmt;
}

(* ------------------------------------------------------------------------- *)
(* Module import/export *)
(* ------------------------------------------------------------------------- *)
and import = 
  | ImportXXX

(* ------------------------------------------------------------------------- *)
(* Toplevel *)
(* ------------------------------------------------------------------------- *)
and item = 
  | IStmt of stmt

  | IFuncDef of function_definition
  | IVarDef of variable_definition
  | IClassDef of class_definition
  | IImport of import

  | IOther of other_item_operator * any list
  and other_item_operator = XXX3

and program = item list

(* ------------------------------------------------------------------------- *)
(* Any *)
(* ------------------------------------------------------------------------- *)
and any =
  | E of expr
  | S of stmt
  | I of item

  | P of program
 (* with tarzan *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let str_of_name = fst
