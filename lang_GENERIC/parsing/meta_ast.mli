(*s: pfff/lang_GENERIC/parsing/meta_ast.mli *)

(*s: signature [[Meta_ast.vof_any]] *)
val vof_any: Ast_generic.any -> Ocaml.v
(*e: signature [[Meta_ast.vof_any]] *)

(*s: signature [[Meta_ast.vof_literal]] *)
(* internals used by other dumpers, e.g., meta_il.ml *)
val vof_literal: Ast_generic.literal -> Ocaml.v
(*e: signature [[Meta_ast.vof_literal]] *)
(*s: signature [[Meta_ast.vof_type_]] *)
val vof_type_: Ast_generic.type_ -> Ocaml.v
(*e: signature [[Meta_ast.vof_type_]] *)
(*s: signature [[Meta_ast.vof_arithmetic_operator]] *)
val vof_arithmetic_operator: Ast_generic.arithmetic_operator -> Ocaml.v
(*e: signature [[Meta_ast.vof_arithmetic_operator]] *)
(*s: signature [[Meta_ast.vof_function_definition]] *)
val vof_function_definition: Ast_generic.function_definition -> Ocaml.v
(*e: signature [[Meta_ast.vof_function_definition]] *)
(*s: signature [[Meta_ast.vof_class_definition]] *)
val vof_class_definition: Ast_generic.class_definition -> Ocaml.v
(*e: signature [[Meta_ast.vof_class_definition]] *)
(*s: signature [[Meta_ast.vof_definition]] *)
val vof_definition: Ast_generic.definition -> Ocaml.v
(*e: signature [[Meta_ast.vof_definition]] *)
(*s: signature [[Meta_ast.vof_directive]] *)
val vof_directive: Ast_generic.directive -> Ocaml.v
(*e: signature [[Meta_ast.vof_directive]] *)
(*s: signature [[Meta_ast.vof_expr]] *)
val vof_expr: Ast_generic.expr -> Ocaml.v
(*e: signature [[Meta_ast.vof_expr]] *)
(*s: signature [[Meta_ast.vof_stmt]] *)
val vof_stmt: Ast_generic.stmt -> Ocaml.v
(*e: signature [[Meta_ast.vof_stmt]] *)

(*e: pfff/lang_GENERIC/parsing/meta_ast.mli *)
