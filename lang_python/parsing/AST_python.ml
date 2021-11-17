(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2011-2015 Tomohiro Matsuyama
 * Copyright (C) 2019-2021 r2c
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
(* Abstract Syntax Tree for Python3.
 *
 * Most of the code in this file derives from code from
 * Tomohiro Matsuyama in ocaml-pythonlib, which itself derives from
 * the official grammar definition of Python.
 *
 * reference: http://docs.python.org/3/library/AST.html
 *
 * See also:
 *  - http://trevorjim.com/python-is-not-context-free/
 *  - https://github.com/gvanrossum/pegen a WIP to write the Python grammar
 *    using a PEG parser
 *
 * Note that this AST supports partly Python2 syntax with the special
 * print and exec statements. It does not support the special tuple
 * parameters syntax though.
 *
 * related work:
 *  - https://github.com/m2ym/ocaml-pythonlib
 *    The original code. The repo was also forked by jeremy buisson
 *    who added a very basic simplifier but remains mostly the same.
 *  - Pyre-check
 *    typechecker and taint-tracker for Python, written in OCaml from facebook
 *  - https://github.com/mattgreen/hython
 *    Python3 interpreter written in Haskell
 *  - libCST (a concrete syntax tree, better for program transformation)
 *    by Instagram
 *
 * history:
 *  - 2019 port to the pfff infrastructure.
 *  - 2019 modified to support types, and many other Python 3 features
 *    (see the python3: tag in this file)
 *  - 2020 backport print and exec statements, to parse some python2 code.
 *
 * todo:
 *  - could use records for all the XxxDef, but what matters now is
 *    AST_generic.ml, which uses records at least.
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
[@@deriving show]

(* a shortcut to annotate some information with token/position information *)
type 'a wrap = 'a * tok
[@@deriving show] (* with tarzan *)

(* round(), square[], curly{}, angle<> brackets *)
type 'a bracket = tok * 'a * tok
[@@deriving show] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

type name = string wrap
[@@deriving show] (* with tarzan *)

(* note that name can be also the special "*" in an import context. *)
type dotted_name = name list
[@@deriving show] (* with tarzan *)

type module_name =
  dotted_name *
  (* https://realpython.com/absolute-vs-relative-python-imports/ *)
  (tok (* . or ... toks *) list) option (* levels, for relative imports *)
[@@deriving show]

(* TODO: reuse AST_generic one? *)
type resolved_name =
  (* this can be computed by a visitor *)
  | LocalVar
  | Parameter
  | GlobalVar
  | ClassField
  (* both dotted_name should contain at least one element! *)
  | ImportedModule of dotted_name
  | ImportedEntity of dotted_name

  (* default case *)
  | NotResolved
[@@deriving show { with_path = false }] (* with tarzan *)

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
type expr =
  | Num of number (* n *)
  | Str of string wrap (* s *)
  (* todo: we should split the token in r'foo' in two, one string wrap
   * for the prefix and a string wrap for the string itself. *)
  | EncodedStr of string wrap * string (* prefix *)
  (* python3: now officially reserved keywords *)
  | Bool of bool wrap
  | None_ of tok

  (* introduce new vars when expr_context = Store *)
  | Name of name (* id *) * expr_context (* ctx *) * resolved_name ref

  | Tuple of expr list_or_comprehension * expr_context
  | List  of expr list_or_comprehension * expr_context
  | DictOrSet of dictorset_elt list_or_comprehension

  (* python3: *)
  | ExprStar of expr (* less: expr_context? always Store anyway no? *)
  (* python3: f-strings
   * reference: https://www.python.org/dev/peps/pep-0498/ *)
  | InterpolatedString of interpolated list
  | ConcatenatedString of interpolated list (* always Str *)

  (* python3: *)
  (* inside an Assign (or ExprStmt) *)
  | TypedExpr of expr * type_

  | BoolOp of boolop wrap (* op *) * expr list (* values *)
  | BinOp of expr (* left *) * operator wrap (* op *) * expr (* right *)
  | UnaryOp of unaryop wrap (* op *) * expr (* operand *)
  | Compare of expr (* left *) * cmpop wrap list (* ops *) * expr list (* comparators *)

  (* note that Python does not have a 'new' keyword, a call with the name
   * of a class is a New *)
  | Call of expr (* func *) * argument list bracket (* args *)

  | Subscript of expr (* value *) * slice list bracket (* slice *) *
                 expr_context

  (* the parameters do not have types here *)
  | Lambda of tok (* lambda *) * parameters (* args *) * tok (* : *) *
              expr (* body *)

  | IfExp of expr (* test *) * expr (* body *) * expr (* orelse *)

  | Yield of tok * expr option (* value *) * bool (* is_yield_from *)
  (* python3: *)
  | Await of tok * expr

  (* python 3.8+; see https://www.python.org/dev/peps/pep-0572/ *)
  | NamedExpr of expr * tok * expr
  | Repr of expr bracket (* `` *)
  (* =~ ObjAccess *)
  | Attribute of expr (* value *) * tok (* . *) * name (* attr *) *
                 expr_context (* ctx *)

  (* sgrep-ext: *)
  | Ellipsis of tok (* should be only in .pyi, types Dict[str,...], or sgrep *)
  | DeepEllipsis of expr bracket
  | TypedMetavar of name * tok * type_
  | ObjAccessEllipsis of expr * tok (* ... *)


and number =
  | Int of int option wrap
  | LongInt of int option wrap
  | Float of float option wrap
  | Imag of string wrap

(* less: could reuse AST_generic.arithmetic_operator *)
and boolop = And | Or

(* the % operator can also be used for strings! "foo %s" % name *)
and operator =
  | Add | Sub | Mult | Div
  | Mod | Pow | FloorDiv
  | LShift | RShift
  | BitOr | BitXor | BitAnd
  | MatMult (* Matrix Multiplication *)

and unaryop = Invert | Not | UAdd | USub

and cmpop =
  | Eq | NotEq
  | Lt | LtE | Gt | GtE
  | Is | IsNot
  | In | NotIn

(* usually a Str or a simple expr.
 * TODO: should also handle format specifier, they are skipped for now
 * during parsing
*)
and interpolated = expr

and 'a list_or_comprehension =
  | CompList of 'a list bracket
  | CompForIf of 'a comprehension

and 'a comprehension = 'a * for_if list
and for_if =
  | CompFor of expr (* introduce new vars *) * (* in *) expr
  | CompIf of expr

and dictorset_elt =
  | KeyVal of expr * expr
  | Key of expr
  (* python3: *)
  | PowInline of expr

(* AugLoad and AugStore are not used *)
and expr_context =
  | Load | Store
  | Del
  | AugLoad | AugStore
  | Param

and slice =
  | Slice of expr option (* lower *) * expr option (* upper *) * expr option (* step *)
  | Index of expr (* value *)

and parameters = parameter list
and parameter =
  (* the first expr can only be a Name or a Tuple (pattern?),
   * and the Name can have a type associated with it
  *)
  | ParamDefault of (name * type_ option) * expr (* default value *)
  (* pattern can be either a name or a tuple pattern *)
  | ParamPattern of param_pattern * type_ option
  | ParamStar of tok (* '*' *)  * (name * type_ option)
  | ParamPow  of tok (* '**' *) * (name * type_ option)
  (* python3: single star delimiter to force keyword-only arguments after.
   * reference: https://www.python.org/dev/peps/pep-3102/ *)
  | ParamSingleStar of tok
  (* python3: single slash delimiter to force positional-only arg prior. *)
  | ParamSlash of tok
  (* sgrep-ext: *)
  | ParamEllipsis of tok

and argument =
  | Arg of expr (* this can be Ellipsis for sgrep *)
  | ArgKwd of name (* arg *) * expr (* value *)
  | ArgStar of (* '*' *)  tok * expr
  | ArgPow  of (* '**' *) tok * expr
  | ArgComp of expr * for_if list

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
(* python3: type annotations!
 * see https://docs.python.org/3/library/typing.html for the semantic
 * and https://www.python.org/dev/peps/pep-3107/ (function annotations)
 * for https://www.python.org/dev/peps/pep-0526/ (variable annotations)
 * for its syntax.
*)
and type_ = expr

(* used in inheritance, to allow default value for metaclass *)
and type_parent = argument

(*****************************************************************************)
(* Pattern *)
(*****************************************************************************)
(* Name, or Tuple? or more? *)
and pattern = expr

(* python2? *)
and param_pattern =
  | PatternName of name
  | PatternTuple of param_pattern list
[@@deriving show { with_path = false }]  (* with tarzan *)

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
type stmt =
  | ExprStmt of expr (* value *)

  (* the left expr should be an lvalue: Name, List, Tuple, Subscript,
   * or Attribute, or ExprStar, which are anything with an expr_context
   * (see also Parser_python.set_expr_ctx).
   * This can introduce new vars.
   * TODO: why take an expr list? can reuse Tuple for tuple assignment
  *)
  | Assign of expr list (* targets *) * tok * expr (* value *)
  | AugAssign of expr (* target *) * operator wrap (* op *) * expr (* value *)

  | For of tok * pattern (* (pattern) introduce new vars *) *
           tok * expr (* 'in' iter *) *
           stmt list (* body *) * stmt list (* orelse *)
  | While of tok * expr (* test *) * stmt list (* body *) *
             stmt list (* orelse *)
  | If of tok * expr (* test *) * stmt list (* body *) *
          stmt list option (* orelse *)
  (* https://docs.python.org/2.5/whatsnew/pep-343.html *)
  | With of tok * expr (* context_expr *) * expr option (* optional_vars *) *
            stmt list (* body *)

  | Return of tok * expr option (* value *)
  | Break of tok | Continue of tok
  | Pass of tok

  | Raise of tok * (expr * expr option (* from *)) option
  | RaisePython2 of tok * expr * expr option (* arguments *) * expr option (* location *)
  | TryExcept of tok * stmt list (* body *) * excepthandler list (* handlers *)
                 * stmt list (* orelse *)
  | TryFinally of tok * stmt list (* body *) * tok * stmt list (* finalbody *)
  | Assert of tok * expr (* test *) * expr option (* msg *)

  | Global of tok * name list (* names *)
  | Delete of tok * expr list (* targets *)
  (* python3: *)
  | NonLocal of tok * name list (* names *)

  (* python2: *)
  | Print of tok * expr option (* dest *) * expr list (* values *) * bool (* nl *)
  | Exec of tok * expr (* body *) * expr option (* glob *) * expr option (* local *)

  (* python3: for With, For, and FunctionDef *)
  | Async of tok * stmt

  | ImportAs   of tok * module_name (* name *) * name option (* asname *)
  | ImportAll  of tok * module_name * tok (* * *)
  | ImportFrom of tok * module_name (* module *) * alias list (* names *)

  (* should be allowed just at the toplevel *)
  | FunctionDef of
    tok (* 'def' *) *
    name (* name *) *
    parameters (* args *) *
    type_ option * (* return type *)
    stmt list (* body *) *
    decorator list (* decorator_list *)

  | ClassDef of
    tok (* 'class' *) *
    name (* name *) *
    type_parent list (* bases *) *
    stmt list (* body *) *
    decorator list (* decorator_list *)


and excepthandler =
    ExceptHandler of
      tok *
      expr option (* type, possibly a list of types as in (Error,Fatal) *) *
      name option (* name, introduce new var, todo: only if pattern is Some *) *
      stmt list (* body *)

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Decorators (a.k.a annotations) *)
(* ------------------------------------------------------------------------- *)
and decorator = tok (* @ *) * dotted_name * argument list bracket option

(* ------------------------------------------------------------------------- *)
(* Function (or method) definition *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Variable definition *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Class definition *)
(* ------------------------------------------------------------------------- *)

(*****************************************************************************)
(* Module *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Module import/export *)
(* ------------------------------------------------------------------------- *)
and alias = name (* name *) * name option (* asname *)
[@@deriving show { with_path = false }]  (* with tarzan *)

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)
type program = stmt list
[@@deriving show]   (* with tarzan *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
type any =
  | Expr of expr
  | Stmt of stmt
  | Stmts of stmt list
  | Program of program

  | DictElem of dictorset_elt
[@@deriving show { with_path = false }] (* with tarzan *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let str_of_name = fst

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)
let context_of_expr = function
  | Attribute (_, _, _, ctx) -> Some ctx
  | Subscript (_, _, ctx) -> Some ctx
  | Name (_, ctx, _)   -> Some ctx
  | List (_, ctx)         -> Some ctx
  | Tuple (_, ctx)        -> Some ctx
  | _                     -> None
