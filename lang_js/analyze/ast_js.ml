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
 *    See 'wrap' below.
 *    update: we actually keep the different kinds of brackets for sgrep, but
 *    they are all agglomerated in a general 'bracket' type. 
 *  - no types (but could revisit this decision)
 *  - no Typescript (no interface)
 *  - no U, B, Yield, Await, Seq, ... just Apply (and Special Id)
 *  - no field vs method. A method is just sugar to define
 *    a field with a lambda (some people even uses directly that forms
 *    thx to arrows).
 *  - old: no Period vs Bracket (actually good to differentiate)
 *  - old: no Object vs Array (actually good to differentiate)
 *  - no func vs method vs arrow, just fun_
 *  - no class elements vs object elements
 *  - No Nop (EmptyStmt); transformed in an empty Block,
 *    (but a new Nop for empty expressions)
 *  - no patterns (they are transpiled, see transpile_js.ml, unless
 *    Ast_js_build.transpile_pattern is false)
 *  - no JSX (see transpile_js.ml, unless Ast_js_build.transpile_xml is false)
 *  - no ForOf (see transpile_js.ml)
 *  - no ExportDefaultDecl, ExportDefaultExpr, just unsugared in
 *    separate variable declarations and an Export name
 *    (using 'default_entity' special name)
 * 
 * todo:
 *  - add back type information? useful for many analysis!
 *  - ast_js_es5.ml? unsugar even more? remove classes, get/set, etc.?
 *  - unsugar ES6 features? lift Var up, rename lexical vars, etc.
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

(* round(), square[], curly{}, angle<> brackets *)
type 'a bracket = tok * 'a * tok
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

(* todo: should rename ident *)
type name = string wrap
 (* with tarzan *)
type ident = string wrap

(* For bar() in a/b/foo.js the qualified_name is 'a/b/foo.bar'. 
 * I remove the filename extension for codegraph (which assumes
 * the dot is a package separator), which is convenient to show 
 * shorter names when exploring a codebase (and maybe also when hovering
 * a function in codemap).
 * This is computed after ast_js_build in graph_code_js.ml
 *)
type qualified_name = string
 (* with tarzan *)

(* todo: use AST_generic.resolved_name at some point, and share the ref! *)
type resolved_name =
  (* this can be computed by ast_js_build.ml *)
  | Local
  | Param
  (* this is computed in graph_code_js.ml in a "naming" phase *)
  | Global of qualified_name
  (* default case *)
  | NotResolved
 (* with tarzan *)

type special = 
  (* Special values *)
  | Null | Undefined (* builtin not in grammar *)

  (* Special vars *)
  | This | Super
  (* CommonJS part1 *)
  | Exports | Module
  (* Asynchronous Module Definition (AMD) *)
  | Define
  (* Reflection *)
  | Arguments

  (* Special apply *)
  | New | NewTarget
  | Eval (* builtin not in grammar *)
  | Seq 
  (* a kind of cast operator: 
   * See https://stackoverflow.com/questions/7452341/what-does-void-0-mean
   *)
  | Void
  | Typeof | Instanceof
  | In | Delete 
  | Spread
  | Yield | YieldStar | Await
  | Encaps of name option (* less: resolve? *)
  (* CommonJS part2 *)
  | Require

  | UseStrict

  | ArithOp of AST_generic.arithmetic_operator
  (* less: should be in statement and unsugared in x+=1 or even x = x + 1 *)
  | IncrDecr of (AST_generic.incr_decr * AST_generic.prefix_postfix)
 (* with tarzan *)

type label = string wrap
 (* with tarzan *)

(* the filename is not "resolved".
 * alt: use a reference like for resolved_name set in graph_code_js.ml and
 * module_path_js.ml? *)
type filename = string wrap
 (* with tarzan *)

(* when doing export default Foo and import Bar, ... *)
let default_entity = "!default!"

type property_name = 
  | PN of name
  (* especially useful for array objects, but also used for dynamic fields *)
  | PN_Computed of expr
  (* less: Prototype *)

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)
and expr =
  | Bool of bool wrap
  | Num of string wrap
  | String of string wrap
  | Regexp of string wrap

  (* For Global the ref is set after ast_js_build in a naming phase in 
   * graph_code_js, hence the use of a ref.
   *)
  | Id of name * resolved_name ref 
  | IdSpecial of special wrap
  | Nop

  (* should be a statement *)
  | Assign of expr * tok * expr

  (* less: could be transformed in a series of Assign(ObjAccess, ...) *)
  | Obj of obj_ 
  | Class of class_ * name option (* when assigned in module.exports  *)
  | ObjAccess of expr * tok * property_name
  (* we could transform it in an Obj but can be useful to remember 
   * the difference in further analysis (e.g., in the abstract interpreter) *)
  | Arr of expr list bracket
  (* this can also be used to access object fields dynamically *)
  | ArrAccess of expr * expr

  | Fun of fun_ * name option (* when recursive or assigned in module.exports*)
  | Apply of expr * expr list

  (* copy-paste of AST_generic.xml (but with different 'expr') *)
  | Xml of xml

  (* could unify with Apply, but need Lazy special then *)
  | Conditional of expr * expr * expr

  (* sgrep-ext: *)
  | Ellipsis of tok
  | DeepEllipsis of expr bracket

    (* transpiled to regular Calls when Ast_js_build.transpile_xml *)
    and xml = {
      xml_tag: ident;
      xml_attrs: (ident * xml_attr_value) list;
      xml_body: xml_body list;
    }
     and xml_attr_value = expr
     and xml_body =
      | XmlText of string wrap
      | XmlExpr of expr
      | XmlXml of xml

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
and stmt = 
  | VarDecl of var

  | Block of stmt list
  | ExprStmt of expr

  | If of tok * expr * stmt * stmt
  | Do of tok * stmt * expr | While of tok * expr * stmt
  | For of tok * for_header * stmt

  | Switch of tok * expr * case list
  | Continue of tok * label option | Break of tok * label option
  | Return of tok * expr

  | Label of label * stmt
 
  | Throw of tok * expr
  | Try of tok * stmt * catch option * (tok * stmt) option

  (* less: ModuleDirective of module_directive 
   * ES6 modules can appear only at the toplevel
  *  but CommonJS require() can be inside ifs
  *)

  (* less: could use some Special instead? *)
  and for_header = 
   | ForClassic of vars_or_expr * expr * expr
   | ForIn of var_or_expr * tok * expr

    (* the expr is usually just an assign *)
    and vars_or_expr = (var list, expr) Common.either
    and var_or_expr = (var, expr) Common.either

  and case = 
   | Case of tok * expr * stmt
   | Default of tok * stmt

  and catch = tok * name * stmt

(*****************************************************************************)
(* Pattern (destructuring binding) *)
(*****************************************************************************)
(* reuse Obj, Arr, etc.
 * transpiled to regular assignments when Ast_js_build.transpile_pattern.
 * sgrep: this is useful for sgrep to keep the ability to match over
 * JS destructuring patterns.
 *)
and pattern = expr

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
and var = { 
  (* can be AST_generic.special_multivardef_pattern when
   * Ast_js_build.transpile_pattern is false with a vinit an Assign itself *)
  v_name: name;
  v_kind: var_kind wrap;
  v_init: expr;
  v_resolved: resolved_name ref;
}
  and var_kind = Var | Let | Const

and fun_ = {
  f_props: fun_prop wrap list;
  f_params: parameter_binding list;
  f_body: stmt;
}
  and parameter_binding =
   | ParamClassic of parameter
   | ParamEllipsis of tok
  and parameter = {
    p_name: name;
    p_default: expr option;
    p_dots: tok option;
  }
  (* less: could transpile *)
  and fun_prop = 
    | Generator | Async
    (* only inside classes *)
    | Get | Set 

and obj_ = property list bracket

and class_ = { 
  (* usually simply an Id *)
  c_extends: expr option;
  c_body: property list bracket;
}
  and property = 
    (* expr is a Fun for methods *)
    | Field of property_name * property_prop wrap list * expr
    (* less: can unsugar? *)
    | FieldSpread of tok * expr
    (* sgrep-ext: used for {fld1: 1, ... } which is distinct from spreading *)
    | FieldEllipsis of tok

  and property_prop =
    | Static
    | Public | Private | Protected

 (* with tarzan *)

(*****************************************************************************)
(* Directives *)
(*****************************************************************************)
(* ES6 module directives appear only at the toplevel. However, for 
 * CommomJS directives, some packages like react have dynamic imports
 * (to select dynamically which code to load depending on whether you run
 * in production or development environment) which means those directives
 * can be inside ifs.
 *)
type module_directive = 
  (* 'name' can be the special Ast_js.default_entity.
   * 'filename' is not "resolved"
   * (you may need for example to add node_modules/xxx/index.js
   * when you do 'import "react"' to get a resolved path).
   * See Module_path_js to resolve paths.
   *)
  | Import of tok * name * name option (* 'name1 as name2' *) * filename
  | Export of name

  (* hard to unsugar in Import because we do not have the list of names *)
  | ModuleAlias of tok * name * filename (* import * as 'name' from 'file' *)

  | ImportCss of filename
  (* those should not exist (except for sgrep where they are useful) *)
  | ImportEffect of tok * filename

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)
type toplevel = 
  | V of var
  (* the tok is for graph_code to build a toplevel entity with a location *)
  | S of tok  * stmt
  | M of module_directive
 (* with tarzan *)

(*****************************************************************************)
(* Program *)
(*****************************************************************************)

type program = toplevel list
 (* with tarzan *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
type any = 
  | Expr of expr
  | Stmt of stmt
  | Item of toplevel
  | Items of toplevel list
  | Program of program
 (* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let str_of_name (s, _) = s
let tok_of_name (_, tok) = tok

let unwrap x = fst x

and string_of_xhp_tag s = s
