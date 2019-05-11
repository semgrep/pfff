(* Yoann Padioleau
 *
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
(* A (real) Abstract Syntax Tree for Javascript, not a Concrete Syntax Tree
 * as in cst_js.ml.
 * 
 * This file contains a simplified Javascript AST. The original
 * Javascript syntax tree (cst_js.ml) is good for code refactoring or
 * code visualization; the types used matches exactly the source. However,
 * for other algorithms, the nature of the CST makes the code a bit
 * redundant. Hence the idea of a real and simplified AST 
 * where certain constructions have been factorized or even removed.
 *
 * Here is a list of the simplications/factorizations:
 *  - no purely syntactical tokens in the AST like parenthesis, brackets,
 *    braces, angles, commas, semicolons, etc. No ParenExpr.
 *    The only token information kept is for identifiers for error reporting.
 *    See wrap() below.
 *
 *  - no types
 *  - no Typescript (no interface)
 *  - no U, B, Yield, Await, Seq, ... just Apply (and Special Id)
 *  - no field vs method. A method is just sugar to define
 *    a field with a lambda (some people even uses directly that forms
 *    thx to arrows).
 *  - no Period vs Bracket
 *  - no Object vs Array
 *  - no func vs method vs arrow
 *  - no class elements vs object elements
 *  - No Nop (EmptyStmt); transformed in an empty Block.
 * 
 * 
 * todo:
 *  - import/export
 *  - add types information
 *  - ast_js_es5.ml? unsugar even more? remove classes, patterns, etc.?
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

type tok = Parse_info.info
type 'a wrap = 'a * tok

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

type name = string wrap

type label = string wrap

type special = 
  (* Special values *)
  | Null | Undefined (* builtin not in grammar *)
  | Nop

  (* Special vars *)
  | This | Super

  (* Special apply *)
  | New | NewTarget
  | Eval (* builtin not in grammar *)
  (* todo? | Require | Import *)
  | Seq
  | Typeof | Instanceof
  | In | Delete | Void 
  | Spread
  | Yield | Await
  | Concat (* Encaps *)

  (* Special apply arithmetic and logic *)
  | Not | And | Or 
  | BitNot | BitAnd | BitOr | BitXor
  | Lsr | Asr | Lsl
  | Equal | PhysEqual 
  | Lower | Greater
  | Plus | Minus | Mul | Div | Mod | Expo

  (* less: should be in statement and unsugared in x+=1 or even x = x + 1 *)
  | Incr of bool (* true = pre *) | Decr of bool


type property_name = 
  | PN of name
  (* especially useful for array objects, but also used for dynamic fields *)
  | PN_Computed of expr
  

(* ------------------------------------------------------------------------- *)
(* Expressions *)
(* ------------------------------------------------------------------------- *)
and expr =
  | Bool of bool wrap
  | Num of string wrap
  | String of string wrap
  | Regexp of string wrap

  | Id of name
  | IdSpecial of special wrap

  | ObjAccess of expr * property_name
  | Apply of expr * expr list
  (* should be a statement *)
  | Assign of expr * expr

  (* could unify with Apply, but need Lazy special then *)
  | Conditional of expr * expr * expr

  | Obj of obj_
  | Fun of fun_
  | Class of class_

(* ------------------------------------------------------------------------- *)
(* Statements *)
(* ------------------------------------------------------------------------- *)
and stmt = 
  | VarDecl of var

  | Block of stmt list
  | ExprStmt of expr

  | If of expr * stmt * stmt
  | Do of stmt * expr | While of expr * stmt
  | For of for_header * stmt

  | Switch of expr * case list
  | Continue of label option | Break of label option
  | Return of expr option

  | Label of label * stmt
 
  | Throw of expr
  | Try of stmt * (string wrap * stmt) option * stmt option

  (* less: could use some Special instead? *)
  and for_header = 
   | ForClassic of (var list, expr) Common.either * expr * expr
   | ForIn of (var, expr) Common.either * expr
   | ForOf of (var, expr) Common.either * expr

  and case = 
   | Case of expr * stmt
   | Default of stmt

(* ------------------------------------------------------------------------- *)
(* Entities *)
(* ------------------------------------------------------------------------- *)

and var = { 
  v_name: name;
  v_kind: var_kind;
  v_init: expr option;
}
  and var_kind = Var | Let | Const

and fun_ = {
  f_props: fun_prop list;
  f_params: parameter list;
  f_body: stmt;
}
  and parameter = {
    p_name: name;
    p_default: expr option;
    p_dots: bool;
  }

  and fun_prop = 
    | Get | Set | Generator | Async

and obj_ = property list

and class_ = { 
  c_extends: expr option;
  c_body: property list;
}

  and property = 
    | Field of property_name * property_prop list * expr
    (* less: can unsugar? *)
    | FieldSpread of expr

  and property_prop =
    | Static
    | Public | Private | Protected

(* ------------------------------------------------------------------------- *)
(* Toplevel *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)

type program = stmt list

(* ------------------------------------------------------------------------- *)
(* Any *)
(* ------------------------------------------------------------------------- *)

type any = 
  | Expr of expr
  | Stmt of stmt
  | Program of program

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
