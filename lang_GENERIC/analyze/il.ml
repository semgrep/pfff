(*s: pfff/lang_GENERIC/analyze/il.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2020 r2c
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
(*e: pad/r2c copyright *)
module G = Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Intermediate Language (IL) for static analysis.
 *
 * Just like for the CST -> AST, the goal of an AST -> IL transformation
 * is to simplify things even more for program analysis purpose.
 *
 * Here are the simplifications done compared to the generic AST:
 *  - intermediate 'instr' type (instr for instruction), for expressions with
 *    side effects and statements without any control flow, 
 *    moving Assign/Seq/Call/Conditional out of 'expr' and
 *    moving Assert out of 'stmt'
 *  - new expression type 'exp' for side-effect free expressions
 *  - intermediate 'lvalue' type; expressions are splitted in 
 *    lvalue vs regular expressions, moved Dot/Index out of expr
 *
 *  - Assign/Calls are now instructions, not expressions, and no more Seq
 *  - no AssignOp, or Decr/Incr, just Assign
 *  - Lambdas are now instructions (not nested again)
 *
 *  - no For/Foreach/DoWhile/While, converted all in Loop, 
 *  - no Foreach, converted in a Loop and 2 new special
 *  - TODO no Switch, converted in Ifs
 *  - TODO no Continue/Break, converted in goto
 *  - less use of expr option (in Return/Assert/...), use Unit in those cases
 *
 *  - no Sgrep constructs
 *  - Naming has been performed, no more ident vs name
 *
 * TODO:
 *   - TODO? have all arguments of Calls be variables?
 *
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
 *  - SIL? in Infer? or more a generic AST than a generic IL?
 *  - Rust IL?
 *  - C-- in OCaml? too low-level?
 *  - LLVM IR (but too far away from original code? complicated 
 *    source maps)
 *  - gcc RTL (too low-level? similar to 3-address code?)
 *  - SiMPL language in BAP/BitBlaze dynamic analysis libraries
 *    but probably too close to assembly/bytecode
 *  - Jimpl in Soot/Wala
 *)

(*****************************************************************************)
(* Token (leaf) *)
(*****************************************************************************)

(*s: type [[Il.tok]] *)
(* the classic *)
type tok = G.tok
(*e: type [[Il.tok]] *)
 (* with tarzan *)
(*s: type [[Il.wrap]] *)
type 'a wrap = 'a G.wrap
(*e: type [[Il.wrap]] *)
 (* with tarzan *)
(*s: type [[Il.bracket]] *)
(* useful mainly for empty containers *)
type 'a bracket = tok * 'a * tok
(*e: type [[Il.bracket]] *)
 (* with tarzan *)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

(*s: type [[Il.ident]] *)
type ident = string wrap
(*e: type [[Il.ident]] *)
 (* with tarzan *)

(*s: type [[Il.name]] *)
(* 'sid' below is the result of name resolution and variable disambiguation
 * using a gensym (see naming_ast.ml). The pair is guaranteed to be 
 * global and unique (no need to handle variable shadowing, block scoping,
 * etc; this has been done already).
 * TODO: use it to also do SSA! so some control-flow insensitive analysis
 * can become control-flow sensitive? (e.g., DOOP)
 * 
 *)
type name = ident * G.sid
(*e: type [[Il.name]] *)
 (* with tarzan *)

(*****************************************************************************)
(* Lvalue *)
(*****************************************************************************)

(* An lvalue, represented as in CIL as a pair. *)
(*s: type [[Il.lval]] *)
type lval = {
  base: base;
  offset: offset;
  (* todo: ltype: typ; *)
}
(*e: type [[Il.lval]] *)
(*s: type [[Il.base]] *)
  and base = 
    | Var of name
    | VarSpecial of var_special wrap
    (* for C *)
    | Mem of exp
(*e: type [[Il.base]] *)

(*s: type [[Il.offset]] *)
  and offset = 
  | NoOffset
  (* What about nested field access? foo.x.y? 
   * - use intermediate variable for that. TODO? same semantic?
   * - do as in CIL and have recursive offset and stop with NoOffset.
   * What about computed field names? 
   * - handle them in Special?
   * - convert in Index with a string exp?
   * Note that Dot is used to access many different kinds of entities:
   *  objects/records (fields), classes (static fields), but also 
   *  packages, modules, namespaces depending on the type of 'var' above.
   *)
  | Dot   of ident
  | Index of exp
(*e: type [[Il.offset]] *)

   (* transpile at some point? *)
(*s: type [[Il.var_special]] *)
   and var_special =
     | This | Super
     | Self | Parent
(*e: type [[Il.var_special]] *)

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(* We use use 'exp' instead of 'expr' to accentuate the difference 
 * with Ast_generic.expr. 
 * Here 'exp' does not contain any side effect!
 *)
(*s: type [[Il.exp]] *)
and exp = {
  e: exp_kind;
  (* todo: etype: typ; *)
  eorig: G.expr;
 } 
(*e: type [[Il.exp]] *)
(*s: type [[Il.exp_kind]] *)
  and exp_kind =
  | Lvalue of lval (* lvalue used in a rvalue context *)
  | Literal of G.literal
  | Composite of composite_kind * exp list bracket
  (* Record could be a Composite where the arguments are CTuple with
   * the Literal (String) as a key, but they are pretty important I think
   * for some analysis so better to support them more directly.
   * TODO should we transform that in a New followed by a series of Assign
   * with Dot? simpler?
   * This could also be used for Dict.
   *)
  | Record of (ident * exp) list
  | Cast of G.type_ * exp
  (* This could be put in call_special, but dumped IL are then less readable
   * (they are too many intermediate _tmp variables then) *)
  | Operator of G.arithmetic_operator wrap * exp list
(*e: type [[Il.exp_kind]] *)

(*s: type [[Il.composite_kind]] *)
 and composite_kind =
  | CTuple
  | CArray | CList | CSet
  | CDict (* could be merged with Record *)
  | Constructor of name (* OCaml *)
(*e: type [[Il.composite_kind]] *)
 (* with tarzan *)

(*s: type [[Il.argument]] *)
type argument = exp
(*e: type [[Il.argument]] *)
 (* with tarzan *)

(*****************************************************************************)
(* Instruction *)
(*****************************************************************************)

(* Easier type to compute lvalue/rvalue set of a too general 'expr', which
 * is now split in  instr vs exp vs lval.
 *)
(*s: type [[Il.instr]] *)
type instr = {
  i: instr_kind;
  iorig: G.expr;
 }
(*e: type [[Il.instr]] *)
(*s: type [[Il.instr_kind]] *)
  and instr_kind =
  (* was called Set in CIL, but a bit ambiguous with Set module *)
  | Assign of lval * exp
  | AssignAnon of lval * anonymous_entity
  | Call of lval option * exp (* less: enforce lval? *) * argument list
  | CallSpecial of lval option * call_special wrap * argument list
  (* todo: PhiSSA! *)
(*e: type [[Il.instr_kind]] *)

(*s: type [[Il.call_special]] *)
  and call_special = 
    | Eval
    (* Note that in some languages (e.g., Python) some regular calls are
     * actually New under the hood.
     * The type_ argument is usually a name, but it can also be an name[] in
     * Java/C++.
     *)
    | New (* TODO: lift up and add 'of type_ * argument list'? *)
    | Typeof | Instanceof | Sizeof
    (* old: better in exp: | Operator of G.arithmetic_operator *)
    | Concat
    | Spread
    | Yield | Await
    (* was in stmt before, but with a new clean 'instr' type, better here *)
    | Assert
    (* was in expr before (only in C/PHP) *)
    | Ref (* TODO: lift up, have AssignRef? *)
    (* when transpiling certain features (e.g., patterns, foreach) *)
    | ForeachNext | ForeachHasNext (* primitives called under the hood *)
(*e: type [[Il.call_special]] *)
    (* | IntAccess of composite_kind * int (* for tuples/array/list *)
       | StringAccess of string (* for records/hashes *)
    *)

(*s: type [[Il.anonymous_entity]] *)
  and anonymous_entity =
    | Lambda of G.function_definition
    | AnonClass of G.class_definition
(*e: type [[Il.anonymous_entity]] *)
 (* with tarzan *)

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
(*s: type [[Il.stmt]] *)
type stmt = {
  s: stmt_kind;
  (* sorig: G.stmt; ?*)
  }
(*e: type [[Il.stmt]] *)
(*s: type [[Il.stmt_kind]] *)
  and stmt_kind =
  | Instr of instr

  (* Switch are converted to a series of If *)
  | If of tok * exp * stmt list * stmt list
  (* While/DoWhile/For are converted in a unified Loop construct.
   * Break/Continue are handled via Label.
   * alt: we could go further and transform in If+Goto, but nice to
   * not be too far from the original code.
   *)
  | Loop of tok * exp * stmt list

  | Return of tok * exp (* use Unit instead of 'exp option' *)

  (* alt: do as in CIL and resolve that directly in 'Goto of stmt' *)
  | Goto of tok * label
  | Label of label

  | Try of stmt list * (name * stmt list) list * stmt list
  | Throw of tok * exp (* less: enforce lval here? *)

  | MiscStmt of other_stmt
(*e: type [[Il.stmt_kind]] *)

(*s: type [[Il.other_stmt]] *)
  and other_stmt = 
    (* everything except VarDef (which is transformed in a Set instr) *)
    | DefStmt of G.definition
    | DirectiveStmt of G.directive
(*e: type [[Il.other_stmt]] *)

(*s: type [[Il.label]] *)
and label = ident * G.sid
(*e: type [[Il.label]] *)
 (* with tarzan *)

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)
(* See ast_generic.ml *)

(*****************************************************************************)
(* Control-flow graph (CFG) *)
(*****************************************************************************)
(* Similar to controlflow.ml, but with a simpler node_kind.
 * See controlflow.ml for more information. *)
(*s: type [[Il.node]] *)
type node = {
  n: node_kind;
  (* old: there are tok in the nodes anyway 
   * t: Parse_info.t option;
   *)
} 
(*e: type [[Il.node]] *)
(*s: type [[Il.node_kind]] *)
  and node_kind = 
    | Enter | Exit 
    | TrueNode | FalseNode (* for Cond *)
    | Join (* after Cond *)

    | NInstr of instr

    | NCond   of tok * exp
    | NReturn of tok * exp
    | NThrow  of tok * exp

    | NOther of other_stmt
(*e: type [[Il.node_kind]] *)
(* with tarzan *)

(*s: type [[Il.edge]] *)
(* For now there is just one kind of edge. Later we may have more, 
 * see the ShadowNode idea of Julia Lawall.
 *)
type edge = Direct 
(*e: type [[Il.edge]] *)

(*s: type [[Il.cfg]] *)
type cfg = (node, edge) Ograph_extended.ograph_mutable
(*e: type [[Il.cfg]] *)

(*s: type [[Il.nodei]] *)
(* an int representing the index of a node in the graph *)
type nodei = Ograph_extended.nodei
(*e: type [[Il.nodei]] *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
(*s: type [[Il.any]] *)
type any = 
  | L of lval
  | E of exp
  | I of instr
  | S of stmt
  | Ss of stmt list
(*  | N of node *)
(*e: type [[Il.any]] *)
 (* with tarzan *)

(*****************************************************************************)
(* L/Rvalue helpers *)
(*****************************************************************************)
(*s: function [[Il.lvar_of_instr_opt]] *)
let lvar_of_instr_opt x =
  match x.i with
  | Assign (lval, _) | AssignAnon (lval, _)
  | Call (Some lval, _, _) | CallSpecial (Some lval, _, _) ->
      (match lval.base with
      | Var n -> Some n
      | VarSpecial _ | Mem _ -> None
      )
  | Call _ | CallSpecial _ -> None
(*e: function [[Il.lvar_of_instr_opt]] *)

(*s: function [[Il.exps_of_instr]] *)
let exps_of_instr x =
  match x.i with
  | Assign (_, exp) -> [exp]
  | AssignAnon _ -> []
  | Call (_, e1, args) -> e1::args
  | CallSpecial (_, _, args) -> args
(*e: function [[Il.exps_of_instr]] *)

(*s: function [[Il.rvars_of_exp]] *)
(* opti: could use a set *)
let rec rvars_of_exp e =
  match e.e with
  | Lvalue ({base=Var var;_}) -> [var]
  | Lvalue _ -> []
  | Literal _ -> []
  | Cast (_, e) -> rvars_of_exp e
  | Composite (_, (_, xs, _)) | Operator (_, xs) -> rvars_of_exps xs 
  | Record ys -> rvars_of_exps (ys |> List.map snd)  
  
and rvars_of_exps xs =
  xs |> List.map (rvars_of_exp) |> List.flatten
(*e: function [[Il.rvars_of_exp]] *)

(*s: function [[Il.rvars_of_instr]] *)
let rvars_of_instr x =
  let exps = exps_of_instr x in
  rvars_of_exps exps
(*e: function [[Il.rvars_of_instr]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: function [[Il.str_of_name]] *)
let str_of_name ((s, _tok), _sid) = s
(*e: function [[Il.str_of_name]] *)

(*s: function [[Il.find_node]] *)
let find_node f cfg =
  cfg#nodes#tolist |> Common.find_some (fun (nodei, node) ->
    if f node then Some nodei else None
  )
(*e: function [[Il.find_node]] *)

(*s: function [[Il.find_exit]] *)
let find_exit cfg = find_node (fun node -> node.n = Exit) cfg
(*e: function [[Il.find_exit]] *)
(*s: function [[Il.find_enter]] *)
let find_enter cfg = find_node (fun node -> node.n = Enter) cfg
(*e: function [[Il.find_enter]] *)

(*e: pfff/lang_GENERIC/analyze/il.ml *)
