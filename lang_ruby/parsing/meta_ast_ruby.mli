val vof_program: Ast_ruby.program -> OCaml.v

val vof_expr: Ast_ruby.expr -> OCaml.v

(* was in ast_ruby_printer.ml before *)
val string_of_expr: Ast_ruby.expr -> string
val string_of_program: Ast_ruby.program -> string
