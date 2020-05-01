(*s: pfff/lang_GENERIC/analyze/naming_ast.mli *)

(*s: signature [[Naming_ast.resolve]] *)
(* Works by side effect on the generic AST by modifying its refs.
 * We pass the lang because some name resolution algorithm may be
 * specific to a language.
 *)
val resolve: Lang.t -> Ast.program -> unit
(*e: signature [[Naming_ast.resolve]] *)
(*e: pfff/lang_GENERIC/analyze/naming_ast.mli *)
