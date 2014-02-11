(* Yoann Padioleau
 *
 * Copyright (C) 2009 University of Urbana Champaign
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This file may seem redundant with the tokens generated by Yacc
 * from parser.mly in parser_c.mli. The problem is that we need for
 * many reasons to remember in the AST the tokens involved in the
 * AST, not just the string, especially for the comment and cpp_passed
 * tokens which are not in the AST at all. So,
 * to avoid recursive mutual dependencies, we provide this file
 * so that Ast_cpp does not need to depend on yacc which depends on
 * Ast_cpp, etc. 
 * 
 * Also, ocamlyacc imposes some stupid constraints on the way we can define
 * the token type. ocamlyacc forces us to do a token type that
 * cant be a pair of a sum type, it must be directly a sum type.
 * We don't have this constraint here.
 * 
 * Also, some yacc tokens are not used in the grammar because they are filtered
 * in some intermediate phases. But they still must be declared because
 * ocamllex may generate them, or some intermediate phase may also
 * generate them (like some functions in parsing_hacks.ml).
 * Here we don't have this problem again so we can have a clearer token type.
 * 
 *)

(*****************************************************************************)
(* constructs put in comments in lexer or parsing_hack *)
(*****************************************************************************)

(* 
 * history: was in ast_cpp.ml before:
 *  "This type is not in the Ast but is associated with the TCommentCpp 
 *  token. I put this enum here because parser_c.mly needs it. I could 
 *  have put it also in lexer_parser."
 * 
 * update: now in token_cpp.ml, and actually right now we want those tokens
 * to be in the AST so that in the matching/transforming of C code, we
 * can detect if some metavariables match code which have some
 * cpp_passed tokens next to them (and so where we should issue a warning).
 *)
type cppcommentkind = 
  | CppDirective 
  | CppAttr 
  | CppMacro 
  | CppMacroExpanded
  | CppPassingNormal (* ifdef 0, cplusplus, etc *) 
  | CppPassingCosWouldGetError (* expr passsing *)
(* TODO  | CppPassingExplicit (* skip_start/end tag *) instead of CppOther? *)
  | CppOther

(* at some point we are supposed to also parse those constructs *)
type cpluspluscommentkind =
  | CplusplusTemplate
  | CplusplusQualifier

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
