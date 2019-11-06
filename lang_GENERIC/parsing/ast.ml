(* 
 * See pfff/h_program-lang/ast_generic.ml.
 *
 * I put ast_generic.ml there because it was used by the other
 * lang_xxx/analyze/xxx_to_generic.ml.
 * This file provides just convenient aliases so one can write in signature
 * files Ast.program instead of the longer Ast_generic.program.
 *)


type program = Ast_generic.program

type any = Ast_generic.any

