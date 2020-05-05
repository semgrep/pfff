(*s: pfff/lang_GENERIC/parsing/ast.ml *)
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
(*s: type [[Ast.xxx]] aliases *)
type program = Ast_generic.program
type name = Ast_generic.name
type ident = Ast_generic.ident
type id_info = Ast_generic.id_info
type expr = Ast_generic.expr
type stmt = Ast_generic.stmt
type any = Ast_generic.any
(*e: type [[Ast.xxx]] aliases *)
(*e: pfff/lang_GENERIC/parsing/ast.ml *)
