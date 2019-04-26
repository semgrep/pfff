(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2012, 2013 Facebook
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
open Common

module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Concrete Syntax Tree for Javascript.
 * 
 * Specification: 
 *  http://www.ecma-international.org/publications/standards/Ecma-262.htm
 * See also 
 *  - https://en.wikipedia.org/wiki/JavaScript
 *  - https://en.wikipedia.org/wiki/ECMAScript
 *  - https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/
 *
 * This AST (and its associated parser) supports most ES6 features:
 *  - classes: a nicer syntax to define classes than objects and prototypes
 *  - import/export: cleaner namespace
 *  - arrows: short lambdas
 *  - default parameters
 *  - SEMI variable number of parameters, e.g. 'function foo(...args)'
 *    and spread of parameters with [ ...arr ]
 *  - template strings (a.k.a interpolated strings), see
 *    https://gist.github.com/lukehoban/9303054#template-strings
 *  - let: lexical vars
 *  - const: immutable declarations
 *  - get/set: sugar to define getter and setter methods
 *  - iterators (for ... of )
 *  - generators (yield, function* ): a nice syntax to define iterators
 *  - optional trailing commas
 * Support also for most ES7-ES9 features:
 *  - SEMI destructuring patterns
 *  - async/await: asynchronous functions and promises TODO
 *
 * See http://es6-features.org/ for explanations of those recent features
 * (and also how they can be converted to ES5 code)
 *
 * This AST (and its associated parser) supports a few more extensions:
 *  - JSX: I am mostly imitating what I have done for XHP in lang_php/,
 *    but with tags possibly containing ':' in their names
 *  - type annotations a la Flow and TypeScript, see
 *    http://en.wikipedia.org/wiki/TypeScript
 *  - interfaces a la Flow and Typescript
 *
 * less:
 *  - imitate https://developer.mozilla.org/en-US/docs/SpiderMonkey/Parser_API
 *
 * related work:
 *  - http://esprima.org/, JS parser in JS
 *  - flow-lang.org, contains now its own parser (it started with this one)
 *  - http://marijnhaverbeke.nl/parse-js/, JS parser in Common Lisp
 *    (which has been since ported to Javascript by the nodejs people)
 *  - jslint, eslint
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

(* Contains among other things the position of the token through
 * the Parse_info.token_location embedded inside it, as well as the
 * transformation field that makes possible spatch on Javascript code.
 *)
type tok = Parse_info.info

(* a shortcut to annotate some information with token/position information *)
and 'a wrap = 'a * tok

and 'a paren   = tok * 'a * tok
and 'a brace   = tok * 'a * tok
and 'a bracket = tok * 'a * tok
and 'a angle = tok * 'a * tok
(* can now have a Right tok at the very end with the trailing comma extension*)
and 'a comma_list = ('a, tok (* the comma *)) Common.either list

(* semicolon. Can be None when was implicitely inserted during parsing *)
and sc = tok option

 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)
type name = string wrap
 (* with tarzan *)

(* facebook-ext: *)
type xhp_tag = string
 (* with tarzan *)

type property_name =
   | PN_String of name
   | PN_Num of string wrap
 (* with tarzan *)

(* es6: note: does not contain the enclosing "'" but the info does *)
type module_path = string wrap
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
type expr =
   | L of litteral
   | V of name
   | This of tok

   (* unop includes new/delete/... *)
   | U of unop wrap * expr
   | B of expr * binop wrap * expr

   | Period of expr * tok (* . *) * name
   (* this is also used to access object computed-fields *)
   | Bracket of expr * expr bracket

   (* can have a trailing comma *)
   | Object of property comma_list brace
   (* The comma_list can have successive Left because of "elison" *)
   | Array of expr comma_list bracket

   (* Call, see also Encaps that is a sort of call when 'name' is not None  *)
   | Apply of expr * expr comma_list paren

   | Conditional of expr * tok (* ? *) * expr * tok (* : *) * expr
   (* bad language, should be in statements *)
   | Assign of expr * assignment_operator wrap * expr
   | Seq of expr * tok (* , *) * expr

   (* nested functions *)
   | Function of func_decl
   (* es6: arrows, a.k.a short lambdas *)
   | Arrow of arrow_func
   (* es6: generators *)
   | Yield of tok * tok option (* '*' *) * expr option
   (* es7: promises *)
   | Await of tok * expr

   (* es6: template (interpolated) strings 
    * less: you can get multiple EncapsString in encaps below; they
    * are not flatten together, to simplify the lexer/parser.
    *)
   | Encaps of name option * tok (* ` *) * encaps list * tok (* ` *)
   (* facebook-ext: *)
   | XhpHtml of xhp_html

   (* unparser: *)
   | Paren of expr paren

     and litteral =
       | Bool of bool wrap
       (* float or int, in decimal/octal/binary/hexadecimal format *)
       | Num of string wrap
       (* less:  | Float of float | Int of int32 *)

       (* does not contain the enclosing "'" but the info does *)
       (* see also XhpText, EncapsString and XhpAttrString *)
       | String of string wrap

       | Regexp of string wrap (* todo? should split the flags *)

       (* There is also Undefined, but this is not considered a reserved
        * keyword. It is treated as a builtin instead.
        * (great language: have not just 1 billion dollar mistake but 2!)
        *)
       | Null of tok

     and unop =
       | U_new | U_delete
       | U_typeof
       | U_void 
       | U_pre_increment  | U_pre_decrement
       | U_post_increment | U_post_decrement
       | U_plus | U_minus | U_not | U_bitnot
       (* es6: spread operator, ...xxx *)
       | U_spread

     and binop =
       | B_instanceof  | B_in

       | B_add  | B_sub
       | B_mul  | B_div  | B_mod  
       | B_le  | B_ge  | B_lt  | B_gt
       | B_lsr  | B_asr  | B_lsl
       | B_equal | B_notequal  | B_physequal  | B_physnotequal
       | B_bitand  | B_bitor  | B_bitxor
       | B_and  | B_or

     and assignment_operator =
       | A_eq
       | A_add  | A_sub
       | A_mul  | A_div  | A_mod  
       | A_lsl  | A_lsr  | A_asr
       | A_and  | A_or | A_xor  

(* ------------------------------------------------------------------------- *)
(* Object properties *)
(* ------------------------------------------------------------------------- *)
   and property =
       (* this includes also methods *)
       | P_field of property_name * tok (* : *) * expr
       (* es6: method notation in object literals too *)
       | P_method of func_decl
       (* es6: similar to OCaml shorthands in records *)
       | P_shorthand of name
       (* es6: inlining of properties/array-elts/string/args *)
       | P_spread of tok (* ... *) * expr

(* ------------------------------------------------------------------------- *)
(* JSX (=~ XHP from PHP) and interporlated strings *)
(* ------------------------------------------------------------------------- *)

 (* facebook-ext: JSX extension, similar to XHP for PHP (hence the name) *)
 and xhp_html =
   | Xhp of xhp_tag wrap * xhp_attribute list * tok (* > *) *
       xhp_body list * xhp_tag option wrap
   | XhpSingleton of xhp_tag wrap * xhp_attribute list * tok (* /> *)

   and xhp_attribute = xhp_attr_name * tok (* = *) * xhp_attr_value
    and xhp_attr_name = string wrap (* e.g. task-bar *)
    and xhp_attr_value =
      | XhpAttrString of string wrap
      | XhpAttrExpr of expr brace
   and xhp_body =
     | XhpText of string wrap
     | XhpExpr of expr option brace
     | XhpNested of xhp_html

 (* es6: template strings (a.k.a. interpolated/encapsulated strings) *)
 and encaps =
 | EncapsString of string wrap
 (* could use 'expr brace', but it's not a regular brace for { *)
 | EncapsExpr of tok (* ${ *) * expr * tok (* } *)

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
and st =
  | VarsDecl of var_kind wrap * var_binding comma_list * sc

  | Block of item list brace
  | Nop of sc
  | ExprStmt of expr * sc

  | If of tok * expr paren * st * (tok (* else *) * st) option
  | Do of tok * st * tok (* while *) * expr paren * sc
  | While of tok * expr paren * st

  | For of tok * tok (* ( *) *
      lhs_or_vars option * tok (* ; *) *
      expr option * tok (* ; *) *
      expr option *
      tok (* ) *) *
      st
  | ForIn of tok * tok (* ( *) * lhs_or_var * tok (* in *) *
      expr * tok (* ) *) * st
  (* es6: iterators *)
  | ForOf of tok * tok (* ( *) * lhs_or_var * tok (* of *) *
      expr * tok (* ) *) * st

  | Switch of tok * expr paren *
      case_clause list brace (* was   (case_clause list * st) list *)

  | Continue of tok * label option * sc
  | Break of tok * label option * sc

  | Return of tok * expr option * sc

  | With of tok * expr paren * st
  | Labeled of label * tok (*:*) * st

  | Throw of tok * expr * sc
  | Try of tok * st (* always a block *) *
      (tok * arg_catch paren * st) option * (* catch *)
      (tok * st) option (* finally *)

  and label = string wrap

  (* less: could unify with 'st', and explain additional constraints *)
  and lhs_or_vars =
    | LHS1 of expr
    | ForVars of (var_kind wrap * var_binding comma_list)

  and lhs_or_var = 
    | LHS2 of expr
    (* the variable_declaration in var_binding  has v_init = None. *)
    | ForVar of (var_kind wrap * var_binding)

  and case_clause =
    | Default of tok * tok (*:*) * item list
    | Case of tok * expr * tok (*:*) * item list

  and arg_catch = string wrap

(* ------------------------------------------------------------------------- *)
(* Type *)
(* ------------------------------------------------------------------------- *)
(* typing-ext: facebook-ext: complex type annotations for Flow/Typescript *)
and type_ =
  (* used for builtin types like 'void', 'number', 'string', 'any/mixed' *)
  | TName of nominal_type
  | TQuestion of tok * type_
  | TFun of param_types * tok (* => *) * type_
  (* comma_list or semicolons_list ?*)
  | TObj of (name * annotation * sc) list brace

(* Most of the time expr is a (V name),
   but Javascript allows qualified names of the form Period(e,tok,name),
   and other ways of dynamically computing types as well.
*)
and nominal_type =
  expr * type_argument comma_list angle option

and type_argument = type_

and type_parameter = name

and type_opt = annotation option

and annotation =
  | TAnnot of tok (* : *) * type_
  | TFunAnnot of type_parameters option * param_types * tok (* : *) * type_

and type_parameters = type_parameter comma_list angle

and param_types = (param_name * annotation) comma_list paren

and param_name =
  | RequiredParam of name
  | OptionalParam of name * tok (* ? *)
  | RestParam of tok (* ... *) * name

(* ------------------------------------------------------------------------- *)
(* Function (or method) definition *)
(* ------------------------------------------------------------------------- *)
and func_decl = {
  f_kind: func_kind;
  f_tok: tok option; (* 'function', but None for methods *)
  f_name: name option; (* None for anonymous functions *)
  (* typing-ext: *)
  f_type_params: type_parameters option;
  f_params: parameter comma_list paren;
  (* typing-ext: *)
  f_return_type: type_opt;
  f_body: item list brace;
}
  and parameter = {
   p_name: name;
  (* typing-ext: *)
   p_type: type_opt;
   (* es6: if not None, then can be followed only by other default parameters 
      or a dots parameter in a parameter comma_list *)
   p_default: default option;
   (* es6: if <> None, then should be last param in a parameter comma_list *)
   p_dots: tok (* ... *) option;
  }
  (* es6: *)
  and default =
  | DNone of tok (* ? *)
  | DSome of tok (* = *) * expr

  and func_kind =
  | Regular
  (* es6: *)
  | Get of tok
  | Set of tok
  (* es6: f_body should contain a 'yield' *)
  | Generator of tok (* '*', but this token is after f_tok *)
  (* es7: f_body should contain a 'await' *)
  | Async of tok

(* es6: arrows.
 * note: we could factorize with func_def, but this would require many
 * elements to become fake tokens, e.g. the parenthesis for parameters
 * when have only one parameter, the braces and semicolon when the body
 * is a simple expression, etc. so simpler to have a a different type.
 *)
and arrow_func = {
  a_params: arrow_params;
  (* typing-ext: *)
  a_return_type: type_opt;
  a_tok: tok (* => *);
  a_body: arrow_body;
 }
 and arrow_params =
 | ASingleParam of parameter
 | AParams of parameter comma_list paren
 and arrow_body =
 | AExpr of expr
 | ABody of item list brace

(* ------------------------------------------------------------------------- *)
(* Variable definition *)
(* ------------------------------------------------------------------------- *)
(* in theory Const and Let can appear only in statement items, not in
 * simple statements (st)
 *)
and var_kind =
  | Var 
  (* es6: *)
  | Const
  | Let

and var_binding = 
 | VarClassic of variable_declaration
 (* es6: *)
 | VarPatternTodo

and variable_declaration = {
  v_name: name;
  v_init: (tok (*=*) * expr) option;
  (* typing-ext: *)
  v_type: type_opt;
}

(* ------------------------------------------------------------------------- *)
(* Pattern (destructuring binding) *)
(* ------------------------------------------------------------------------- *)
(* es6: TODO see VarPatternTodo *)

(* ------------------------------------------------------------------------- *)
(* Class definition *)
(* ------------------------------------------------------------------------- *)
(* es6: finally classes built in the language *)
and class_decl = {
  c_tok: tok; (* 'class' *)
  c_name: name;
  (* typing-ext: *)
  c_type_params: type_parameter comma_list angle option;
  c_extends: (tok (* extends *) * nominal_type) option;
  c_body: class_stmt list brace;
}

  and class_stmt =
  | Method of static_opt * func_decl
  | Field of name * annotation * sc
  (* unparser: *)
  | ClassExtraSemiColon of sc

  and static_opt = tok option (* static *)

(* ------------------------------------------------------------------------- *)
(* Interface definition *)
(* ------------------------------------------------------------------------- *)
(* typing-ext: not in regular JS *)
and interface_decl = {
  i_tok: tok; (* 'interface' *)
  i_name: name;
  i_type_params: type_parameter comma_list angle option;
  i_type: type_;
}

(* ------------------------------------------------------------------------- *)
(* A Statement list item (often just at the toplevel) *)
(* ------------------------------------------------------------------------- *)
and item =
  (* contains VarsDecl, which are a Decl *)
  | St of st

  | FunDecl of func_decl
  (* es6-ext: *)
  | ClassDecl of class_decl
  (* typing-ext: *)
  | InterfaceDecl of interface_decl

(* ------------------------------------------------------------------------- *)
(* Module *)
(* ------------------------------------------------------------------------- *)
(* es6: *)
and import =
  | ImportFrom of (import_clause * (tok (* from *) * module_path))
  (* import for its side effects only *)
  | ImportEffect of module_path

  (* less: if have Some, Some there is a comma between, but the token
   * is not present in the AST.
   *)
  and import_clause = import_default option * name_import option
   and name_import =
     | ImportNamespace of tok (* * *) * tok (* as *) * name
     | ImportNames of import_name comma_list brace
   and import_default = name
   and import_name = name * (tok (* as *) * name) option

(* es6: *)
and export = 
  (* St in item can only be a VarsDecl *)
  | ExportDecl of item
  (* item can be only FunDecl or ClassDecl, no St *)
  | ExportDefaultDecl of tok (* default *) * item
  | ExportDefaultExpr of tok (* default *) * expr * sc

  | ExportNames of import_name comma_list brace * sc
  (* reexport *)
  | ReExportNamespace of tok (* * *) * (tok (* from *) * module_path) * sc
  | ReExportNames of import_name comma_list brace * 
                      (tok (* from *) * module_path) * sc
  

(* ------------------------------------------------------------------------- *)
(* Toplevel *)
(* ------------------------------------------------------------------------- *)

 and module_item = 
  | It of item
  (* es6-ext: *)
  | Import of (tok (* import *) * import * sc)
  | Export of (tok (* export *) * export)

 and program = module_item list
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Any *)
(* ------------------------------------------------------------------------- *)

type any =
  | Expr of expr
  | Stmt of st
  | Item of item
  | Program of program
 (* with tarzan *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let unwrap = fst
let unparen (_,x,_) = x
let unbrace = unparen
let unbracket = unparen

let uncomma xs = Common.map_filter (function
  | Left e -> Some e
  | Right _info -> None
  ) xs

let info_of_name (_s, info) = info

(*****************************************************************************)
(* Abstract line *)
(*****************************************************************************)

(* When we have extended the AST to add some info about the tokens,
 * such as its line number in the file, we can not use anymore the
 * ocaml '=' operator to compare Ast elements. To overcome this problem, to
 * be able to use again '=', we just have to get rid of all those extra
 * information, to "abstract those line" (al) information.
 *)

let al_info x =
  { x with PI.token = PI.Ab }

(*****************************************************************************)
(* Views *)
(*****************************************************************************)

(* examples:
 * inline more static funcall in expr type or variable type
 *)

(*****************************************************************************)
(* Helpers, could also be put in lib_parsing.ml instead *)
(*****************************************************************************)

let fakeInfoAttach info =
  let info = PI.rewrap_str "';' (from ASI)" info in
  let pinfo = PI.token_location_of_info info in
  { PI.
    token = PI.FakeTokStr (";", Some (pinfo, -1));
    transfo = PI.NoTransfo;
  }

let remove_quotes_if_present s =
  (* for JX the entity names are passed as strings when
   * defining the entity (e.g. JX.Install('Typeahead'. ...)
   * but use normally (e.g. var x = JX.Typeahead(...))
   * so here we normalize.
   *)
  match s with
  | _ when s =~ "'\\(.*\\)'$" -> Common.matched1 s
  | _ when s =~ "\"\\(.*\\)\"$" -> Common.matched1 s
  | _ -> s
