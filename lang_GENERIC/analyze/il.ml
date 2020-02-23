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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Intermediate language for static analysis.
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
 *  - RIL, The Ruby Intermediate Language, Furr et al, DSL'09
 *  - C-- in OCaml?
 *  - Rust IL?
 *  - LLVM IR (but too far away from original code? complicated 
 *    source maps)
 *)
module G = Ast_generic

(*****************************************************************************)
(* Token (leaf) *)
(*****************************************************************************)

type tok = G.tok
type 'a wrap = 'a G.wrap
type 'a bracket = 'a G.bracket

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

(* The string is the result of name resolution and variable disambiguation
 * using gensym. The string is guaranteed to be global and unique 
 * (no need to handle variable shadowing, block scoping, etc; this has 
 * been done already).
 *)
type var = string wrap * var_info
  (* similar to G.id_info *)
  and var_info = {
   (* the refs below are shared with Ast_generic.id_info, so modifying them
    * will by side effect modify also the refs in the generic AST.
    *)
    var_resolved: G.resolved_name option ref; 
    var_type: G.type_ option ref;

    var_orig: G.name option; (* None for temporary variables *)
  }

(*****************************************************************************)
(* Lvalue *)
(*****************************************************************************)

type lval = 
  | Var of var
  (* computed field names are not handled here but in special *)
  | Dot of var * ident
  | Index of var * exp
  (* only C *)
  | Deref of var * tok

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

and exp = 
  | Literal of G.literal
  | Composite of composite_kind * exp list bracket
  | Lvalue of lval
  | Cast of type_ * exp

 and composite_kind =
  | Tuple
  | Array | List | Set
  | Dict

and argument = exp

(*****************************************************************************)
(* Instruction *)
(*****************************************************************************)

and instr =
  | Set of lval * exp
  | Call of lval option * var * argument list
  | Special of lval option * special_kind * argument list

(*****************************************************************************)
(* Statemement *)
(*****************************************************************************)
(* See ast_generic.ml *)

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)
(* See ast_generic.ml *)
