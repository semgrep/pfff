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
module G = Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Intermediate Language (IL) for static analysis.
 *
 * Just like for the CST -> AST, the goal of an AST -> IL transformation
 * is to simplify things even more for program analysis purpose.
 *
 * TODO Here are simplifications that could be done to the AST:
 *  - intermediate instr type (instr for instruction), for statements without
 *    any control flow
 *  - intermediate lvalue type, expressions are splitted in 
 *    lvalue vs regular expressions
 *  - Assign is now an instruction, not an expression
 *  - no AssignOp, or Decr/Incr, just Assign
 *  - Calls are now instructions (not nested inside complex expressions)
 *    and all its arguments are variables?
 *  - Naming has been performed, no more ident vs name
 *  - Lambdas are now instructions (not nested again)
 *  - Seq are instructions
 * 
 * Note that we still want to be close to the original code so that
 * error reported on the IL can be mapped back to error on the original code
 * (source "maps").
 *
 * history:
 *  - cst_php.ml (was actually called ast_php.ml)
 *  - ast_php.ml (was called ast_php_simple.ml)
 *  - pil.ml, still for PHP
 *  - il.ml for AST generic
 * 
 * related work:
 *  - CIL, C Intermediate Language, Necula et al, CC'00
 *  - RIL, The Ruby Intermediate Language, Furr et al, DLS'09
 *  - C-- in OCaml?
 *  - Rust IL?
 *  - LLVM IR (but too far away from original code? complicated 
 *    source maps)
 *  - SiMPL language in BAP/BitBlaze dynamic analysis libraries
 *    but probably too close to assembly/bytecode
 *)

(*****************************************************************************)
(* Token (leaf) *)
(*****************************************************************************)

(* the classic *)
type tok = G.tok
 (* with tarzan *)
type 'a wrap = 'a G.wrap
 (* with tarzan *)
(* useful mainly for empty containers *)
type 'a bracket = tok * 'a * tok
 (* with tarzan *)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

type ident = string wrap
 (* with tarzan *)

(* 'sid' below is the result of name resolution and variable disambiguation
 * using a gensym (see naming_ast.ml). The pair is guaranteed to be 
 * global and unique (no need to handle variable shadowing, block scoping,
 * etc; this has been done already).
 * TODO: use it to also do SSA! so some control-flow insensitive analysis
 * can become control-flow sensitive! (e.g., DOOP)
 * 
 *)
type var = ident * G.sid
 (* with tarzan *)

(* for constructors and other global entities.
 * 'sid' below should be the result of global name resolution using 
 * codegraph or something similar.
 *)
type name = ident * G.sid
 (* with tarzan *)

(*****************************************************************************)
(* Lvalue *)
(*****************************************************************************)

(* An lvalue, represented as in CIL as a pair. *)
type lval = {
  base: base;
  offset: offset;
  (* todo: ltype: typ; *)
}
  and base = 
    | Var of var
    | VarSpecial of var_special wrap
    (* for C *)
    | Mem of exp

  and offset = 
  | NoOffset
  (* What about nested field access? foo.x.y? 
   * - use intermediate variable for that. TODO? same semantic?
   * - do as in CIL and have recursive offset and stop with NoOffset.
   * What about computed field names? 
   * - handle them in Special?
   * - convert in Index?
   * Note that Dot is used to access many different kinds of entities:
   *  objects (fields), classes (static fields), but also packages, modules,
   *  namespaces depending on the type of 'var' above.
   *)
  | Dot   of ident
  | Index of exp

   (* transpile at some point? *)
   and var_special =
     | This | Super
     | Self | Parent

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(* We use use 'exp' instead of 'expr' to accentuate the difference 
 * with Ast_generic.expr. 
 * Here 'exp' does not contain any side effect!
 *)
and exp = {
  e: exp_kind;
  (* todo: etype: typ; *)
  eorig: G.expr;
 } 
  and exp_kind =
  | Literal of G.literal
  | Composite of composite_kind * exp list bracket
  | Lvalue of lval
  | Cast of G.type_ * exp

 and composite_kind =
  | Tuple
  | Array | List
  | Dict
  | Constructor of name (* OCaml *)
 (* with tarzan *)

type argument = exp
 (* with tarzan *)

(*****************************************************************************)
(* Instruction *)
(*****************************************************************************)

(* Easier type to compute lvalue/rvalue set of a too general 'expr', which
 * is now split in  instr vs exp vs lval.
 *)
type instr = {
  i: instr_kind;
  iorig: G.expr;
 }
  and instr_kind =
  | Set of lval * exp
  | SetAnon of lval * anonymous_entity
  | Call of lval option * exp * argument list
  | CallSpecial of lval option * call_special wrap * argument list
  (* todo: PhiSSA! *)

  and call_special = 
    | Eval
    | New
    | Typeof | Instanceof | Sizeof
    | Operator of G.arithmetic_operator | Concat
    | Spread
    | Yield | Await
    | Assert
    (* when transpiling certain features *)
    | TupleAccess of int (* when transpiling tuples *)
    (* only in C/PHP *)
    | Ref

  and anonymous_entity =
    | Lambda of G.function_definition
    | AnonClass of G.class_definition
 (* with tarzan *)

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
type stmt = {
  s: stmt_kind;
  (* sorig: G.stmt; ?*)
  }
  and stmt_kind =
  | Instr of instr

  | If of tok * exp * stmt list * stmt list
  (* less: could be transpiled? *)
  | Switch of tok * exp * case_and_body list
  (* While/DoWhile/For are converted in unified Loop construct.
   * Break/Continue are handled via Label 
   *)
  | Loop of tok * exp * stmt list

  | Return of tok * exp (* use Unit instead of 'exp option' *)

  (* alt: do as in CIL and resolve that directly in 'Goto of stmt' *)
  | Goto of tok * label
  | Label of label * stmt list

  | Try of stmt list * (var * stmt list) list * stmt list
  
  (* everything except VarDef which should be transformed in a Set *)
  | DefStmt of G.definition
  | DirectiveStmt of G.directive

   and case_and_body = case list * stmt list
     and case = 
       | Case of tok * exp
       | Default of tok

and label = ident * G.sid
 (* with tarzan *)

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)
(* See ast_generic.ml *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
type any = 
  | L of lval
  | E of exp
  | I of instr
  | S of stmt
  | Ss of stmt list
 (* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
