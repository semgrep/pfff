
val vof_any: Ast_generic.any -> Ocaml.v

(* internals used by other dumpers, e.g., meta_il.ml *)
val vof_literal: Ast_generic.literal -> Ocaml.v
val vof_type_: Ast_generic.type_ -> Ocaml.v
val vof_arithmetic_operator: Ast_generic.arithmetic_operator -> Ocaml.v
val vof_function_definition: Ast_generic.function_definition -> Ocaml.v
val vof_class_definition: Ast_generic.class_definition -> Ocaml.v
val vof_definition: Ast_generic.definition -> Ocaml.v
val vof_directive: Ast_generic.directive -> Ocaml.v
val vof_expr: Ast_generic.expr -> Ocaml.v
val vof_stmt: Ast_generic.stmt -> Ocaml.v

