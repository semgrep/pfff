(* 
 * See pfff/h_program-lang/ast_generic.ml.
 *
 * I put ast_generic.ml under h_program-lang/ because it was used by the other
 * lang_xxx/analyze/xxx_to_generic.ml. I needed to break the mutual 
 * dependency between the lang_xxx and lang_GENERIC by moving some
 * basic files of lang_GENERIC/parsing under h_program-lang.
 * ast.ml provides just convenient aliases so one can write in signature
 * files Ast.program instead of the longer Ast_generic.program.
 *)


type program = Ast_generic.program

type any = Ast_generic.any

