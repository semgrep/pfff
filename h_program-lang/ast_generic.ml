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
 * (e.g., scheck, sgrep, checked_return). 
 *
 * rational: In the end, programming languages have a lot in common.
 * Even though most interesting analysis are probably better done on a
 * per-language basis, many useful analysis are trivial and require just an
 * AST and a visitor. One could duplicate those analysis for each language
 * or design a generic AST (this file) to be generic enough to
 * factorize all those analysis (e.g., unused entity). The goal is
 * still to remain as precise as possible, not as generic as ast_fuzzy.ml
 * for example or a very general but imprecise tree of nodes.
 * 
 * TODO:
 *  - later: add Go (easy)
 *  - later: add Ruby, Rust, Scala (difficult)
 *  - later: add C++ (argh)
 *  - see ast_fuzzy.ml TODOs for ideas to use ast_generic for sgrep.
 *
 * related work:
 *  - ast_fuzzy.ml (in this directory)
 *  - github semantic
 *    https://github.com/github/semantic
 *  - Coverity common program representation?
 *  - Semmle internal common representation?
 *  - Infer SIL (for C++, Java, Objective-C)
 *  - Dawson Engler and Fraser Brown micro-checkers for multiple languages
 *  - https://tabnine.com/ which supports multiple languages
 *  - Lightweight Multi-language syntax transformation paper 
 *  - srcML https://www.srcml.org/doc/srcMLGrammar.html
 *    but just for C/C++/C#/Java and seems pretty heavy
 *
 * design choices to have a generic data structure:
 *  - add some 'a, 'b, 'c around expr/stmt/...
 *  - functorize and add some type hole (type tstmt; type texpr; ...)
 *  - data-type a la carte like in github-semantic but Seems too high-level
 *    with astronaut-style architecture (too abstract, too advanced features).
 *  - the OtherXxx strategy used in this file (simple)
 *
 * history:
 *  - started with crossproduct of Javascript, Python, PHP, Java, and C
 *    (and a bit of OCaml)
 *
 * invariants:
 *  - all the other_xxx types should contain only simple constructors (enums)
 *    without any parameter. I rely on that to simplify the code 
 *    of the generic mapper and matcher.
 *  - each language should add the VarDefs that defines the locals
 *    used in a function (instead of having the first Assign play the role
 *    of a VarDef, as done in Python for example).
 *  - to correctly compute a CFG (Control Flow Graph), the stmt type 
 *    should list all constructs that contains other statements and 
 *    try to avoid to use the very generic OtherXxx of any
 *  - to correctly compute a DFG (Data Flow Graph), each constructs that
 *    introduce a new variable should have a relevant comment 'newvar:'
 *
 * See also pfff/lang_GENERIC/
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

(* a shortcut to annotate some information with position information *)
type 'a wrap = 'a * tok
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Names *)
(* ------------------------------------------------------------------------- *)

type ident = string wrap
 (* with tarzan *)

type dotted_ident = ident list (* at least 1 element *)
 (* with tarzan *)

(* todo? module_name * name? 
 * todo? not enough in OCaml with functor and type arguments or C++ templates? 
*)
type qualifier = dotted_ident
 (* with tarzan *)

(* can also be used for packages, or namespaces *)
type module_name =
  | FileName of string wrap   (* ex: Javascript import, C #include *)
  | DottedName of dotted_ident (* ex: Python *)
 (* with tarzan *)

(* todo: see also scope_code.ml *)
type resolved_name =
  | Local (* TODO of gensym *)
  | Param (* TODO of gensym *)
  | Global of dotted_ident (* or just name? *) (* can also use 0 for gensym *)

  | NotResolved

  | Macro
  | EnumConstant
  | ImportedModule of dotted_ident

 (* with tarzan *)

(* big mutually recursive types because of the use of 'any' in OtherXxx *)

type name = ident * id_info
  and id_info =
  { id_qualifier: qualifier option;
    id_typeargs: type_arguments option; (* Java *)
    id_resolved: resolved_name ref; (* variable tagger (naming) *)
    id_type: type_ option ref; (* type checker (typing) *)
  }

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

and expr = 
  (* basic (atomic) values *)
  | L of literal

  (* composite values *)
  | Container of container_operator * expr list
  | Tuple of expr list (* special case of Container *) 

  (* And-type (field.vinit should be a Some) *)
  | Record of field list
  (* Or-type (could be used instead of Container, Cons, Nil, etc.) *)
  | Constructor of name * expr list

  (* very special value *)
  | Lambda of function_definition
  (* usually an argument of a New (used in Java, Javascript) *)
  | AnonClass of class_definition

  | Nop (* less: could be merged with L Unit *)

  (* todo: newvar: sometimes abused to also introduce a newvar (as in Python)
   * but ultimately those cases should be rewritten to first introduce a
   * VarDef
   *)
  | Name of name
  | IdSpecial of special wrap

  (* operators and function application *)
  | Call of expr * arguments
  (* (XHP, JSX, TSX), could transpile also *)
  | Xml of xml
  (* IntepolatedString of expr list is simulated with a 
   * Call(IdSpecial (Concat ...)) *)

  (* The left part should be an lvalue (id, ObjAccess, ArrayAccess, Deref)
   * but it can also be a pattern (Tuple, Container), but
   * you should really use LetPattern for that.
   * Assign can also be abused to declare new variables, but you should use
   * variable_definition for that.
   * less: should be in stmt, but many languages allow this at expr level 
   *)
  | Assign of expr * expr
  (* less: should desugar in Assign, should be only binary_operator *)
  | AssignOp of expr * arithmetic_operator wrap * expr
  (* newvar:! *)
  | LetPattern of pattern * expr

  (* can also be used for Record, Class, or Module access depending on expr *)
  | ObjAccess of expr * ident
  | ArrayAccess of expr * expr (* less: slice *)

  | Conditional of expr * expr * expr
  | MatchPattern of expr * action list
  (* less: TryFunctional *)

  | Yield of expr
  | Await of expr

  | Cast of type_ * expr
  (* less: should be in statement *)
  | Seq of expr list

  (* less: could be in Special *)
  | Ref of expr (* &, address of *)
  | DeRef of expr (* '*' *)

  | Ellipses of tok (* for sgrep, and also types in Python *)

  | OtherExpr of other_expr_operator * any list

  and literal = 
    | Bool of bool wrap
    | Int of string wrap | Float of string wrap
    | Char of string wrap | String of string wrap | Regexp of string wrap
    | Unit of tok (* a.k.a Void *) | Null of tok | Undefined of tok (* JS *)

  and container_operator = 
    (* Tuple was lifted up *)
    | Array (* todo? designator? *)
    | List | Set
    | Dict (* a.k.a Hash or Map (combine with Tuple to get Key/value pair) *)


  and special = 
   (* special vars *)
   | This | Super
   | Self | Parent (* different from This/Super? *)

   (* special apply *)
   | Eval
   | Typeof | Instanceof | Sizeof
   (* note that certain languages do not have a 'new' keyword (e.g., Python),
    * instead certain 'Call' are really 'New' *)
   | New  (* usually associated with Call(New, [ArgType _;...]) *)

   | Concat (* used for interpolated strings constructs *)
   | Spread (* inline list var, in Container or call context *)

   | ArithOp of arithmetic_operator
   (* should be lift up and transformed in Assign at stmt level *)
   | IncrDecr of (incr_decr * prefix_postfix)

    (* mostly binary operator 
     * less: could be divided in really Arith vs Logical (bool) operators,
     * but see is_boolean_operator() helper below.
     * Note that Mod can be used for %style string formatting in Python.
     * todo? use a Special operator intead for that? but need type info?
     *)
    and arithmetic_operator = 
      | Plus (* unary too *) | Minus (* unary too *) 
      | Mult | Div | Mod
      | Pow | FloorDiv (* Python *)
      | LSL | LSR | ASR (* L = logic, A = Arithmetic, SL = shift left *) 
      | BitOr | BitXor | BitAnd | BitNot (* unary *)
      (* todo? rewrite in CondExpr? have special behavior *)
      | And | Or (* also shortcut operator *) | Xor (* PHP*) | Not (* unary *)
      | Eq     | NotEq     (* less: could be desugared to Not Eq *)
      | PhysEq | NotPhysEq (* less: could be desugared to Not PhysEq *)
      | Lt | LtE | Gt | GtE  (* less: could be desugared to Or (Eq Lt) *)
    and incr_decr = Incr | Decr
    and prefix_postfix = Prefix | Postfix

  and action = pattern * expr

  (* TODO *)
  and xml = any list

  and arguments = argument list
    and argument =
      (* regular argument *)
      | Arg of expr (* can be Call (IdSpecial Spread, Id foo) *)
      (* keyword argument *)
      | ArgKwd of ident * expr
      (* type argument for New, instanceof/sizeof/typeof, C macros *)
      | ArgType of type_

      | ArgOther of other_argument_operator * any list

       and other_argument_operator =
        (* Python *)
        | OA_ArgPow (* a kind of Spread, but for Dict instead of List *)
        | OA_ArgComp (* comprehension *)
        (* OCaml *)
        | OA_ArgQuestion


  and other_expr_operator = 
    (* Javascript *)
    | OE_Exports | OE_Module 
    | OE_Define | OE_Arguments 
    | OE_NewTarget
    | OE_Delete | OE_YieldStar
    | OE_Encaps (* less: convert to regular funcall? *)
    | OE_Require (* todo: lift to Import? *) 
    | OE_UseStrict (* less: lift up to program attribute/directive? *)
    | OE_ObjAccess_PN_Computed (* less: convert to ArrayAccess *)
    (* Python *)
    | OE_Imag
    | OE_Is | OE_IsNot (* less: could be part of a set_operator? or PhysEq? *)
    | OE_In | OE_NotIn (* less: could be part of a obj_operator? *)
    | OE_Invert
    | OE_Slice | OE_SliceIndex | OE_SliceRange
    (* TODO: newvar: *)
    | OE_CompForIf | OE_CompFor | OE_CompIf
    | OE_CmpOps
    | OE_Repr
    (* Java *)
    | OE_NameOrClassType | OE_ClassLiteral | OE_NewQualifiedClass
    (* C *)
    | OE_GetRefLabel
    | OE_ArrayInitDesignator | OE_GccConstructor (* transform in New? *)
    (* PHP *)
    | OE_Unpack
    (* OCaml *)
    | OE_FieldAccessQualified | OE_RecordWith 
    | OE_StmtExpr (* OCaml has just expressions, no statements *)

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
and stmt =
  (* later: lift Call/Assign/Seq here *)
  | ExprStmt of expr

  | DefStmt of definition
  | DirectiveStmt of directive

  | Block of stmt list

  | If of expr * stmt * stmt
  | While of expr * stmt
  | DoWhile of stmt * expr
  | For of for_header * stmt

  (* less: could be merged with ExprStmt (MatchPattern ...) *)
  | Switch of expr * case_and_body list

  | Return of expr
  | Continue of expr option | Break of expr option (* todo? switch to label? *)

  | Label of label * stmt
  | Goto of label

  | Throw of expr (* a.k.a raise *)
  | Try of stmt * catch list * finally option
  | Assert of expr * expr option (* message *)

  | OtherStmt of other_stmt_operator * any list

  and case_and_body = case list * stmt
   (* less: could be merged with pattern *)
    and case  =
    | Case of expr
    | Default

  and catch = pattern * stmt
  and finally = stmt

  and label = ident

  and for_header = 
    | ForClassic of for_var_or_expr list (* init *) * 
                    expr (* cond *) * 
                    expr (* next *)
    (* newvar: *)
    | ForEach of pattern * expr (* pattern 'in' expr *)

    and for_var_or_expr = 
    (* newvar: *)
    | ForInitVar of entity * variable_definition
    | ForInitExpr of expr

  and other_stmt_operator = 
    (* Python *)
    | OS_Delete 
    | OS_ForOrElse | OS_WhileOrElse | OS_TryOrElse
    | OS_With | OS_ThrowFrom | OS_ThrowNothing | OS_Global | OS_NonLocal
    | OS_Pass
    | OS_Async
    (* Java *)
    | OS_Sync
    (* C *)
    | OS_Asm

(*****************************************************************************)
(* Pattern *)
(*****************************************************************************)
and pattern = 
  | PatLiteral of literal
  (* Or-Type *)
  | PatConstructor of name * pattern list
  (* And-Type *)
  | PatRecord of field_pattern list

  (* newvar:! *)
  | PatVar of ident

  (* special cases of PatConstructor *)
  | PatTuple of pattern list
  | PatList of pattern list
  | PatKeyVal of pattern * pattern (* a kind of PatTuple *)

  (* special case of PatVar *)
  | PatUnderscore of tok

  (* OCaml *)
  | PatDisj  of pattern * pattern
  | PatTyped of pattern * type_
  | PatWhen  of pattern * expr
  | PatAs    of pattern * ident

  | OtherPat of other_pattern_operator * any list

  and field_pattern = name * pattern

  and other_pattern_operator =
  (* Python *)
  | OP_Expr (* todo: should transform in pattern when can *)
  (* Javascript *)
  | OP_Var (* todo: should transform in pattern when can *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

and type_ =
  (* todo? a type_builtin = TInt | TBool | ...? see Literal *)
  | TyBuiltin of string wrap (* int, bool, etc. could be TApply with no args *)
  | TyFun of type_ list (* use parameter? args (not curried) *) * 
             type_ (* return type *)
  (* covers tuples, list, etc. and also regular typedefs *)
  | TyApply of name * type_arguments
  | TyVar of ident (* typedef? no type variable in polymorphic type *)

  (* a special case of TApply, also a special case of TPointer *)
  | TyArray of (* const_expr *) expr option * type_
  | TyPointer of type_
  | TyTuple of type_ list
  | TyQuestion of type_ (* option type *)

  | OtherType of other_type_operator * any list
  
  and type_arguments = type_argument list

    and type_argument = 
      | TypeArg of type_

      | OtherTypeArg of other_type_argument_operator * any list

      and other_type_argument_operator =
       | OTA_Question

  and other_type_operator = 
  (* Python *)
  | OT_Expr | OT_Arg (* todo: should transform in type_ when can *)
  (* C *)
  | OT_StructName | OT_UnionName | OT_EnumName
  (* PHP *)
  | OT_Shape | OT_Variadic

(* ------------------------------------------------------------------------- *)
(* Attribute *)
(* ------------------------------------------------------------------------- *)
(* a.k.a decorators, annotations *)
and attribute = 
  | Static | Volatile | Extern
  (* for class fields *)
  | Public | Private | Protected
  | Abstract | Final
  (* for vars (JS) *)
  | Var | Let
  (* for fields *)
  | Mutable | Const
  (* for functions *)
  | Generator | Async 
  | Recursive | MutuallyRecursive
  (* for methods *)
  | Ctor | Dtor
  | Getter | Setter
  (* for parameters *)
  | Variadic
  (* for general @annotations *)
  | NamedAttr of ident * any list

  | OtherAttribute of other_attribute_operator * any list

  and other_attribute_operator = 
    (* Java *)
    | OA_StrictFP | OA_Transient | OA_Synchronized | OA_Native
    | OA_AnnotJavaOther
    | OA_AnnotThrow
    (* Python *)
    | OA_Expr (* todo: should transform in NamedAttr when can *)

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
and definition = entity * definition_kind (* (or decl) *)

  and entity = {
    name: ident;
    attrs: attribute list;
    type_: type_ option; (* less: use ref to enable typechecking *)
    tparams: type_parameter list;
  }

  (* can have empty "body" when the definition is actually a declaration
   * in a header file *)
  and definition_kind =
    | FuncDef   of function_definition (* valid for methods too *)
    (* newvar: *)
    | VarDef    of variable_definition  (* valid for constants and fields too *)

    | TypeDef   of type_definition
    | ClassDef  of class_definition

    | ModuleDef of module_definition
    | MacroDef of macro_definition
    | Signature of type_

(* template/generics/polymorphic *)
and type_parameter = ident * type_parameter_constraints

  and type_parameter_constraints = type_parameter_constraint list

   and type_parameter_constraint = 
     | Extends of type_
 
(* ------------------------------------------------------------------------- *)
(* Function (or method) definition *)
(* ------------------------------------------------------------------------- *)
(* less: could be merged with variable_definition *)
and function_definition = {
 (* less: could be merged in entity.type_ *)
 fparams: parameters;
 frettype: type_ option; (* return type *)
 fbody: stmt;
}
  and parameters = parameter list
    (* newvar: *)
    and parameter =
     | ParamClassic of parameter_classic
     | ParamPattern of pattern

     | OtherParam of other_parameter_operator * any list

    (* less: could be merged with variable_definition, or pattern *)
    and parameter_classic = { 
     pname: ident;
     pdefault: expr option;
     ptype: type_ option;
     pattrs: attribute list;
    }
  and other_parameter_operator =
     (* Python *)
     | OPO_KwdParam
     (* PHP *)
     | OPO_Ref (* less: or encode in type? *)

(* ------------------------------------------------------------------------- *)
(* Variable definition *)
(* ------------------------------------------------------------------------- *)
(* Also used for constant_definition with attrs = [Const].
 * Also used for field definition in a class (and record).
 * Could also use for function_definition with vinit = Some (Lambda (...))
 *)
and variable_definition = {
  (* less: could remove function_definition as expr can be a Lambda but maybe
   * useful to explicitely makes the difference for now? *)
  vinit: expr option;
  (* less: could merge in entity.type_ *)
  vtype: type_ option;
}

(* ------------------------------------------------------------------------- *)
(* Type definition *)
(* ------------------------------------------------------------------------- *)
and type_definition = {
   tbody: type_definition_kind;
  }

  and type_definition_kind = 
   | OrType  of or_type_element list  (* enum/ADTs *)           
   (* field.vtype should be defined here *)
   | AndType of field list (* record/struct/union *) 

   | AliasType of type_
   | Exception of ident (* same name than entity *) * type_ list

   | OtherTypeKind of other_type_kind_operator * any list

  and or_type_element =
    | OrConstructor of ident * type_ list
    | OrEnum of ident * expr
    | OrUnion of ident * type_

    | OtherOr of other_or_type_element_operator * any list

      and other_or_type_element_operator =
      (* Java *)
      | OOTEO_EnumWithMethods | OOTEO_EnumWithArguments

  (* Field definition and use, for classes and records *)

 (* less: could be merged with variable_definition,
  * I don't call it field_definition because it's used both to
  * define the shape of a field (a definition), and when creating
  * an actual field (a value)
  *)
  and field = 
    | FieldVar of entity * variable_definition
    | FieldMethod of entity * function_definition

    | FieldDynamic of expr (* dynamic name *) * attribute list * expr (* value*)
    | FieldSpread of expr (* usually a Name *)

    | FieldStmt of stmt

  and other_type_kind_operator = 
     (* C *)
     | OTKO_EnumWithValue (* obsolete actually now that has OrEnum *)
     (* OCaml *)
     | OTKO_AbstractType

(* ------------------------------------------------------------------------- *)
(* Class definition *)
(* ------------------------------------------------------------------------- *)
(* less: could be a special kind of type_definition *)
and class_definition = {
  ckind: class_kind;
  cextends: type_ list; 
  cimplements: type_ list;
  cbody: field list;
}
  and class_kind = 
    | Class
    | Interface
    | Trait

(* ------------------------------------------------------------------------- *)
(* Module definition (a.k.a package, a.k.a namespace)  *)
(* ------------------------------------------------------------------------- *)
and module_definition = {
  mbody: module_definition_kind;
}

  and module_definition_kind =
    | ModuleAlias of name
    | ModuleStruct of dotted_ident option * item list

    | OtherModule of other_module_operator * any list

  and other_module_operator =
   (* OCaml *)
   | OMO_Functor

(* ------------------------------------------------------------------------- *)
(* Macro definition *)
(* ------------------------------------------------------------------------- *)
and macro_definition = {
  macroparams: ident list;
  macrobody: any list;
}

(*****************************************************************************)
(* Directives (Module import/export, macros) *)
(*****************************************************************************)
and directive = 
  | Import    of module_name * alias list
  | ImportAll of module_name * ident option (* as name *)

  | OtherDirective of other_directive_operator * any list

  and alias = ident * ident option (* as name *)

  and other_directive_operator = 
  (* Javascript *)
  | OI_Export | OI_ImportCss | OI_ImportEffect

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)
(* less: should merge stmt, item, and field? *)
and item = 
  | IStmt of stmt

  (* could be removed since they are as LocalDef and LocalDirective in stmt *)
  | IDef of definition
  | IDir of directive

and program = item list

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

(* mentioned in many OtherXxx so must be part of the mutually recursive type *)
and any =
  | Id of ident
  | N of name
  | En of entity

  | E of expr
  | S of stmt
  | T of type_
  | P of pattern

  | Def of definition
  | Dir of directive
  | I of item

  | Pa of parameter
  | Ar of argument
  | At of attribute
  | Dk of definition_kind
  | Di of dotted_ident
  | Fld of field

  | Pr of program

 (* with tarzan *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let str_of_name = fst

(*****************************************************************************)
(* Error *)
(*****************************************************************************)

(* this can be used in the xxx_to_generic.ml file to signal limitations *)
exception Error of string * Parse_info.t

let error tok msg = 
  raise (Error (msg, tok))

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let empty_info () = {
   id_qualifier = None;
   id_typeargs = None;
   id_resolved = ref NotResolved;
   id_type = ref None;
 }

let basic_param id = { 
    pname = id;
    pdefault = None;
    ptype = None;
    pattrs = [];
}

let basic_entity id attrs = {
  name = id;
  attrs = attrs;
  type_ = None;
  tparams = [];
}

let basic_field id typeopt =
  let entity = basic_entity id [] in
  FieldVar (entity, { vinit = None; vtype = typeopt})

let empty_var () = 
  { vinit = None; vtype = None }

let expr_to_arg e = 
  Arg e

let entity_to_param { name; attrs; type_; tparams = _unused } = 
  { pname = name;
    pdefault = None;
    ptype = type_;
    pattrs = attrs;
  }

let opt_to_nop opt =
  match opt with
  | None -> Nop
  | Some e -> e

let opt_to_name opt =
  match opt with
  | None -> "FakeNAME", Parse_info.fake_info "FakeNAME"
  | Some n -> n

let stmt1 xs =
  match xs with
  | [] -> Block []
  | [st] -> st
  | xs -> Block xs

(* todo? sign that should merge field with item and stmt? *)
let stmt_to_field st = 
  match st with
  | DefStmt (entity, VarDef def) -> FieldVar (entity, def)
  | DefStmt (entity, FuncDef def) -> FieldMethod (entity, def)
  | _ -> FieldStmt st

(* less: could be a Block containing LocalDef or LocalDirective *)
let stmt_to_item st =
  match st with
  | DefStmt def -> IDef def
  | DirectiveStmt dir -> IDir dir
  | _ -> IStmt st

let is_boolean_operator = function
 | Plus (* unary too *) | Minus (* unary too *) 
 | Mult | Div | Mod
 | Pow | FloorDiv (* Python *)
 | LSL | LSR | ASR (* L = logic, A = Arithmetic, SL = shift left *) 
 | BitOr | BitXor | BitAnd | BitNot (* unary *)
  -> false
 | And | Or | Xor | Not
 | Eq     | NotEq     
 | PhysEq | NotPhysEq 
 | Lt | LtE | Gt | GtE 
   -> true
