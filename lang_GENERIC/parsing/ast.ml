(*s: pfff/lang_GENERIC/parsing/ast.ml *)
(*s: type [[Ast.program]] *)
(* 
 * See pfff/h_program-lang/ast_generic.ml.
 *
 * I put ast_generic.ml under h_program-lang/ because it is used by the other
 * lang_xxx/analyze/xxx_to_generic.ml files. We need to break the mutual 
 * dependency between the lang_xxx/ and lang_GENERIC/ by moving some
 * basic files of lang_GENERIC/parsing/ under h_program-lang/.
 *
 * This file provides just convenient aliases so one can write in signature
 * files Ast.program instead of the longer Ast_generic.program.
 *)

type program = Ast_generic.program
(*e: type [[Ast.program]] *)

(*s: type [[Ast.name]] *)
type name = Ast_generic.name
(*e: type [[Ast.name]] *)
(*s: type [[Ast.ident]] *)
type ident = Ast_generic.ident
(*e: type [[Ast.ident]] *)
(*s: type [[Ast.id_info]] *)
type id_info = Ast_generic.id_info
(*e: type [[Ast.id_info]] *)
(*s: type [[Ast.expr]] *)
type expr = Ast_generic.expr
(*e: type [[Ast.expr]] *)
(*s: type [[Ast.stmt]] *)
type stmt = Ast_generic.stmt
(*e: type [[Ast.stmt]] *)

(*s: type [[Ast.any]] *)
type any = Ast_generic.any
(*e: type [[Ast.any]] *)
(*e: pfff/lang_GENERIC/parsing/ast.ml *)
